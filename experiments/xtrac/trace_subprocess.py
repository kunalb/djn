#!/usr/bin/env python3
"""
BCC script to trace subprocess calls (fork, clone, exec) from a process tree
and output as a binary Perfetto trace file.

Usage:
    sudo ./trace_subprocess.py -p <pid>       # Trace existing process and children
    sudo ./trace_subprocess.py -c "command"   # Run command and trace it
    sudo ./trace_subprocess.py -a             # Trace all processes

Output: subprocess_trace.perfetto (binary Perfetto trace)

Requires BCC (system packages, not pip):
    Ubuntu/Debian: sudo apt install bpfcc-tools python3-bpfcc
    Fedora:        sudo dnf install bcc-tools python3-bcc
    Arch:          sudo pacman -S bcc bcc-tools python-bcc
"""

try:
    from bcc import BPF
except ImportError:
    import sys
    print("Error: BCC (BPF Compiler Collection) is not installed.", file=sys.stderr)
    print()
    print("Install for your distro:", file=sys.stderr)
    print("  Ubuntu/Debian: sudo apt install bpfcc-tools python3-bpfcc", file=sys.stderr)
    print("  Fedora:        sudo dnf install bcc-tools python3-bcc", file=sys.stderr)
    print("  Arch:          sudo pacman -S bcc bcc-tools python-bcc", file=sys.stderr)
    sys.exit(1)
import argparse
import ctypes
import os
import signal
import struct
import subprocess
import sys
import time
from collections import defaultdict

# BPF program
BPF_PROGRAM = r"""
#include <uapi/linux/ptrace.h>
#include <linux/sched.h>
#include <linux/fs.h>

#define ARGSIZE 128
#define MAXARG 20
#define COMM_SIZE 16

enum event_type {
    EVENT_FORK = 1,
    EVENT_EXEC = 2,
    EVENT_EXIT = 3,
};

struct event_t {
    u64 timestamp_ns;
    u32 pid;
    u32 ppid;
    u32 tgid;
    u32 uid;
    int retval;
    enum event_type type;
    char comm[COMM_SIZE];
    char filename[ARGSIZE];
};

BPF_PERF_OUTPUT(events);
BPF_HASH(traced_pids, u32, u8);

static __always_inline int should_trace(u32 pid, u32 ppid) {
    // Check if we're tracing all processes
    u32 key = 0;
    u8 *trace_all = traced_pids.lookup(&key);
    if (trace_all && *trace_all == 1) {
        return 1;
    }

    // Check if this pid or its parent is in our trace set
    u8 *val = traced_pids.lookup(&pid);
    if (val) return 1;

    val = traced_pids.lookup(&ppid);
    if (val) return 1;

    return 0;
}

static __always_inline void add_to_trace(u32 pid) {
    u8 val = 1;
    traced_pids.update(&pid, &val);
}

// Trace fork/clone via sched_process_fork tracepoint
TRACEPOINT_PROBE(sched, sched_process_fork) {
    u32 parent_pid = args->parent_pid;
    u32 child_pid = args->child_pid;

    if (!should_trace(parent_pid, 0)) {
        return 0;
    }

    // Add child to traced set
    add_to_trace(child_pid);

    struct event_t event = {};
    event.timestamp_ns = bpf_ktime_get_ns();
    event.pid = child_pid;
    event.ppid = parent_pid;
    event.tgid = child_pid;
    event.type = EVENT_FORK;

    bpf_get_current_comm(&event.comm, sizeof(event.comm));

    events.perf_submit(args, &event, sizeof(event));
    return 0;
}

// Trace execve syscall entry
TRACEPOINT_PROBE(syscalls, sys_enter_execve) {
    u64 pid_tgid = bpf_get_current_pid_tgid();
    u32 pid = pid_tgid >> 32;
    u32 tgid = pid_tgid;

    struct task_struct *task = (struct task_struct *)bpf_get_current_task();
    u32 ppid = 0;
    bpf_probe_read(&ppid, sizeof(ppid), &task->real_parent->tgid);

    if (!should_trace(pid, ppid)) {
        return 0;
    }

    struct event_t event = {};
    event.timestamp_ns = bpf_ktime_get_ns();
    event.pid = pid;
    event.ppid = ppid;
    event.tgid = tgid;
    event.type = EVENT_EXEC;
    event.uid = bpf_get_current_uid_gid();

    bpf_get_current_comm(&event.comm, sizeof(event.comm));
    bpf_probe_read_user_str(&event.filename, sizeof(event.filename), args->filename);

    events.perf_submit(args, &event, sizeof(event));
    return 0;
}

// Trace process exit
TRACEPOINT_PROBE(sched, sched_process_exit) {
    u64 pid_tgid = bpf_get_current_pid_tgid();
    u32 pid = pid_tgid >> 32;

    struct task_struct *task = (struct task_struct *)bpf_get_current_task();
    u32 ppid = 0;
    bpf_probe_read(&ppid, sizeof(ppid), &task->real_parent->tgid);

    if (!should_trace(pid, ppid)) {
        return 0;
    }

    struct event_t event = {};
    event.timestamp_ns = bpf_ktime_get_ns();
    event.pid = pid;
    event.ppid = ppid;
    event.type = EVENT_EXIT;

    bpf_get_current_comm(&event.comm, sizeof(event.comm));

    events.perf_submit(args, &event, sizeof(event));

    // Remove from traced set
    traced_pids.delete(&pid);
    return 0;
}
"""


class PerfettoTraceWriter:
    """
    Writes events in Perfetto trace format (protobuf-based).

    Perfetto trace format uses nested protobuf messages. We generate the raw
    bytes directly to avoid requiring the full perfetto protobuf definitions.

    Supports live streaming - the trace file is valid at any point and can be
    opened in Perfetto UI while still being written.
    """

    # Protobuf wire types
    WIRETYPE_VARINT = 0
    WIRETYPE_FIXED64 = 1
    WIRETYPE_LENGTH_DELIMITED = 2
    WIRETYPE_FIXED32 = 5

    # Perfetto trace.proto field numbers
    TRACE_PACKET_FIELD = 1

    # TracePacket field numbers
    TIMESTAMP_FIELD = 8
    TRUSTED_PACKET_SEQUENCE_ID_FIELD = 10
    TRACK_EVENT_FIELD = 11
    TRACK_DESCRIPTOR_FIELD = 60

    # TrackEvent field numbers
    TRACK_EVENT_TYPE_FIELD = 9
    TRACK_UUID_FIELD = 11
    TRACK_EVENT_NAME_FIELD = 23

    # TrackDescriptor field numbers
    TRACK_UUID_DESC_FIELD = 1
    TRACK_NAME_FIELD = 2
    PROCESS_DESCRIPTOR_FIELD = 3

    # ProcessDescriptor field numbers
    PROCESS_PID_FIELD = 1
    PROCESS_NAME_FIELD = 6

    # TrackEvent types
    TYPE_SLICE_BEGIN = 1
    TYPE_SLICE_END = 2
    TYPE_INSTANT = 3

    def __init__(self, filename=None):
        self.track_uuids = {}
        self.sequence_id = 1
        self.base_time = None
        self.packet_count = 0
        self.file = None
        self.filename = filename

    def open(self, filename=None):
        """Open the trace file for streaming writes."""
        if filename:
            self.filename = filename
        if self.filename:
            self.file = open(self.filename, 'wb')
            print(f"Streaming trace to: {self.filename}")

    def close(self):
        """Close the trace file."""
        if self.file:
            self.file.close()
            self.file = None
            print(f"Wrote {self.packet_count} packets to {self.filename}")

    def __enter__(self):
        self.open()
        return self

    def __exit__(self, exc_type, exc_val, exc_tb):
        self.close()
        return False

    def _encode_varint(self, value):
        """Encode an integer as a varint."""
        bits = value & 0x7f
        value >>= 7
        result = b''
        while value:
            result += bytes([0x80 | bits])
            bits = value & 0x7f
            value >>= 7
        result += bytes([bits])
        return result

    def _encode_field(self, field_number, wire_type, value):
        """Encode a protobuf field."""
        tag = (field_number << 3) | wire_type
        return self._encode_varint(tag) + value

    def _encode_varint_field(self, field_number, value):
        """Encode a varint field."""
        return self._encode_field(
            field_number,
            self.WIRETYPE_VARINT,
            self._encode_varint(value)
        )

    def _encode_string_field(self, field_number, value):
        """Encode a string/bytes field."""
        if isinstance(value, str):
            value = value.encode('utf-8')
        length = self._encode_varint(len(value))
        return self._encode_field(
            field_number,
            self.WIRETYPE_LENGTH_DELIMITED,
            length + value
        )

    def _encode_message_field(self, field_number, value):
        """Encode an embedded message field."""
        length = self._encode_varint(len(value))
        return self._encode_field(
            field_number,
            self.WIRETYPE_LENGTH_DELIMITED,
            length + value
        )

    def _write_packet(self, packet):
        """Write a packet to the trace file immediately."""
        packet_bytes = self._encode_message_field(self.TRACE_PACKET_FIELD, packet)
        if self.file:
            self.file.write(packet_bytes)
            self.file.flush()  # Flush immediately for live viewing
        self.packet_count += 1

    def _get_track_uuid(self, pid):
        """Get or create a track UUID for a process."""
        if pid not in self.track_uuids:
            self.track_uuids[pid] = hash(f"process_{pid}") & 0xFFFFFFFFFFFFFFFF
        return self.track_uuids[pid]

    def add_process_descriptor(self, pid, name):
        """Add a process track descriptor (written immediately)."""
        uuid = self._get_track_uuid(pid)

        # ProcessDescriptor
        proc_desc = (
            self._encode_varint_field(self.PROCESS_PID_FIELD, pid) +
            self._encode_string_field(self.PROCESS_NAME_FIELD, name)
        )

        # TrackDescriptor
        track_desc = (
            self._encode_varint_field(self.TRACK_UUID_DESC_FIELD, uuid) +
            self._encode_string_field(self.TRACK_NAME_FIELD, f"Process {pid}: {name}") +
            self._encode_message_field(self.PROCESS_DESCRIPTOR_FIELD, proc_desc)
        )

        # TracePacket
        packet = (
            self._encode_varint_field(self.TRUSTED_PACKET_SEQUENCE_ID_FIELD, self.sequence_id) +
            self._encode_message_field(self.TRACK_DESCRIPTOR_FIELD, track_desc)
        )

        self._write_packet(packet)

    def add_event(self, timestamp_ns, pid, event_type, name, is_end=False):
        """Add a track event (written immediately)."""
        if self.base_time is None:
            self.base_time = timestamp_ns

        relative_ts = timestamp_ns - self.base_time
        uuid = self._get_track_uuid(pid)

        if event_type == "instant":
            te_type = self.TYPE_INSTANT
        elif is_end:
            te_type = self.TYPE_SLICE_END
        else:
            te_type = self.TYPE_SLICE_BEGIN

        # TrackEvent
        track_event = (
            self._encode_varint_field(self.TRACK_EVENT_TYPE_FIELD, te_type) +
            self._encode_varint_field(self.TRACK_UUID_FIELD, uuid) +
            self._encode_string_field(self.TRACK_EVENT_NAME_FIELD, name)
        )

        # TracePacket
        packet = (
            self._encode_varint_field(self.TIMESTAMP_FIELD, relative_ts) +
            self._encode_varint_field(self.TRUSTED_PACKET_SEQUENCE_ID_FIELD, self.sequence_id) +
            self._encode_message_field(self.TRACK_EVENT_FIELD, track_event)
        )

        self._write_packet(packet)


class SubprocessTracer:
    """Traces subprocess creation using BCC/eBPF."""

    EVENT_FORK = 1
    EVENT_EXEC = 2
    EVENT_EXIT = 3

    def __init__(self, target_pid=None, trace_all=False, output_file="subprocess_trace.perfetto"):
        self.target_pid = target_pid
        self.trace_all = trace_all
        self.output_file = output_file
        self.events = []
        self.processes = {}  # pid -> info
        self.running = True
        self.bpf = None
        self.perfetto = None

    def _handle_event(self, cpu, data, size):
        """Handle incoming BPF events."""
        event = self.bpf["events"].event(data)

        event_data = {
            'timestamp_ns': event.timestamp_ns,
            'pid': event.pid,
            'ppid': event.ppid,
            'tgid': event.tgid,
            'type': event.type,
            'comm': event.comm.decode('utf-8', errors='replace').rstrip('\x00'),
            'filename': event.filename.decode('utf-8', errors='replace').rstrip('\x00'),
        }

        self.events.append(event_data)

        # Print event
        event_type_str = {
            self.EVENT_FORK: "FORK",
            self.EVENT_EXEC: "EXEC",
            self.EVENT_EXIT: "EXIT",
        }.get(event_data['type'], "UNKNOWN")

        if event_data['type'] == self.EVENT_FORK:
            print(f"[{event_type_str}] ppid={event_data['ppid']} -> pid={event_data['pid']} ({event_data['comm']})")
            self.perfetto.add_process_descriptor(event_data['pid'], event_data['comm'])
            self.perfetto.add_event(
                event_data['timestamp_ns'],
                event_data['ppid'],
                "instant",
                f"fork -> {event_data['pid']}"
            )
            self.processes[event_data['pid']] = {
                'start_time': event_data['timestamp_ns'],
                'comm': event_data['comm'],
            }

        elif event_data['type'] == self.EVENT_EXEC:
            filename = event_data['filename'] or event_data['comm']
            print(f"[{event_type_str}] pid={event_data['pid']} exec({filename})")
            self.perfetto.add_event(
                event_data['timestamp_ns'],
                event_data['pid'],
                "slice",
                f"exec: {os.path.basename(filename)}"
            )
            if event_data['pid'] in self.processes:
                self.processes[event_data['pid']]['comm'] = os.path.basename(filename)

        elif event_data['type'] == self.EVENT_EXIT:
            print(f"[{event_type_str}] pid={event_data['pid']} ({event_data['comm']})")
            self.perfetto.add_event(
                event_data['timestamp_ns'],
                event_data['pid'],
                "slice",
                f"exit",
                is_end=True
            )

    def run(self):
        """Start tracing with live streaming to output file."""
        print("Loading BPF program...")
        self.bpf = BPF(text=BPF_PROGRAM)

        # Initialize traced_pids map
        traced_pids = self.bpf["traced_pids"]

        # Open perfetto writer for streaming
        self.perfetto = PerfettoTraceWriter(self.output_file)
        self.perfetto.open()

        if self.trace_all:
            # Use key 0 with value 1 to indicate trace-all mode
            traced_pids[ctypes.c_uint32(0)] = ctypes.c_uint8(1)
            print("Tracing all processes")
        elif self.target_pid:
            traced_pids[ctypes.c_uint32(self.target_pid)] = ctypes.c_uint8(1)
            print(f"Tracing PID {self.target_pid} and its children")

            # Add process descriptor for the initial process
            try:
                with open(f"/proc/{self.target_pid}/comm", 'r') as f:
                    comm = f.read().strip()
            except:
                comm = "unknown"
            self.perfetto.add_process_descriptor(self.target_pid, comm)

        # Set up perf event handling
        self.bpf["events"].open_perf_buffer(self._handle_event)

        print("Tracing... Press Ctrl+C to stop.")
        print(f"Trace is being written live - you can open {self.output_file} at any time")
        print("-" * 60)

        def signal_handler(sig, frame):
            self.running = False

        signal.signal(signal.SIGINT, signal_handler)

        try:
            while self.running:
                try:
                    self.bpf.perf_buffer_poll(timeout=100)
                except KeyboardInterrupt:
                    break
        finally:
            # Close the trace file
            self.perfetto.close()

        print("-" * 60)
        print(f"Captured {len(self.events)} events")

        return self.events


def main():
    parser = argparse.ArgumentParser(
        description="Trace subprocess creation and output Perfetto trace",
        formatter_class=argparse.RawDescriptionHelpFormatter,
        epilog="""
Examples:
    sudo ./trace_subprocess.py -p 1234          # Trace PID 1234 and children
    sudo ./trace_subprocess.py -c "make -j4"    # Run make and trace it
    sudo ./trace_subprocess.py -a               # Trace all processes
    sudo ./trace_subprocess.py -o my_trace.perfetto -c "./build.sh"
        """
    )
    parser.add_argument('-p', '--pid', type=int, help='PID to trace (and its children)')
    parser.add_argument('-c', '--command', help='Command to run and trace')
    parser.add_argument('-a', '--all', action='store_true', help='Trace all processes')
    parser.add_argument('-o', '--output', default='subprocess_trace.perfetto',
                       help='Output file (default: subprocess_trace.perfetto)')

    args = parser.parse_args()

    if os.geteuid() != 0:
        print("Error: This script must be run as root (requires CAP_BPF)", file=sys.stderr)
        sys.exit(1)

    target_pid = None
    child_proc = None

    if args.command:
        # Start the command and trace it
        print(f"Starting command: {args.command}")
        child_proc = subprocess.Popen(
            args.command,
            shell=True,
            preexec_fn=os.setsid
        )
        target_pid = child_proc.pid
        print(f"Started process with PID {target_pid}")
    elif args.pid:
        target_pid = args.pid
    elif not args.all:
        parser.print_help()
        sys.exit(1)

    tracer = SubprocessTracer(target_pid=target_pid, trace_all=args.all, output_file=args.output)

    try:
        tracer.run()
    finally:
        if child_proc:
            # Clean up child process
            try:
                os.killpg(os.getpgid(child_proc.pid), signal.SIGTERM)
            except:
                pass

    print(f"\nTrace written to: {args.output}")
    print("Open with: https://ui.perfetto.dev/")


if __name__ == "__main__":
    main()
