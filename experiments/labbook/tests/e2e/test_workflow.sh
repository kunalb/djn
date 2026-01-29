#!/bin/bash
# E2E tests for lab CLI - full workflow test

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
source "$SCRIPT_DIR/helpers.sh"

# Change to project root
cd "$SCRIPT_DIR/../.."

# Build first
echo "Building lab..."
cargo build --quiet

echo ""
echo "=============================="
echo "Lab CLI E2E Tests"
echo "=============================="
echo ""

# Setup
setup_test_env
trap cleanup_test_env EXIT

cd "$TEST_DIR/source"

# ============================================
# Test: lab init
# ============================================
echo ""
echo "--- Testing: lab init ---"

# Should fail before init
assert_failure "status fails before init" lab status

# Initialize with defaults (non-interactive for testing)
export LAB_DIR="$TEST_DIR/lab-notes"
mkdir -p "$LAB_DIR"
cd "$LAB_DIR"
git init
git config user.email "test@example.com"
git config user.name "Test User"
cd "$TEST_DIR/source"

# Create .lab config manually for testing
mkdir -p .lab
cat > .lab/config.yaml << EOF
lab_dir: $LAB_DIR
repo_name: source
artifacts:
  - "*.txt"
notes_template: |
  ---
  tags: []
  hypothesis:
  result:
  ---
  ## Notes
git_hook: false
EOF

assert_file_exists "config.yaml created" ".lab/config.yaml"
assert_file_contains "config has lab_dir" ".lab/config.yaml" "lab_dir"

# ============================================
# Test: lab status (no experiment)
# ============================================
echo ""
echo "--- Testing: lab status (no experiment) ---"

assert_output_contains "status shows no experiment" "No experiment" lab status

# ============================================
# Test: lab open
# ============================================
echo ""
echo "--- Testing: lab open ---"

# Create a test artifact
echo "test content" > output.txt

# Open experiment (with EDITOR=true to skip interactive editing)
EDITOR=true lab open baseline

assert_file_exists ".lab/current exists" ".lab/current"
assert_file_contains "current has experiment id" ".lab/current" "baseline"

# Check experiment was created in lab dir
TODAY=$(date +%Y/%m/%d)
assert_dir_exists "experiment dir created" "$LAB_DIR/$TODAY"

# Find the experiment dir
EXP_DIR=$(find "$LAB_DIR/$TODAY" -type d -name "*baseline*" | head -1)
assert_dir_exists "baseline experiment exists" "$EXP_DIR"
assert_file_exists "meta.yaml exists" "$EXP_DIR/meta.yaml"
assert_file_exists "notes.md exists" "$EXP_DIR/notes.md"
assert_file_contains "meta has status open" "$EXP_DIR/meta.yaml" "status: open"

# ============================================
# Test: lab status (with experiment)
# ============================================
echo ""
echo "--- Testing: lab status (with experiment) ---"

assert_output_contains "status shows experiment" "baseline" lab status
assert_output_contains "status shows open" "Open" lab status

# ============================================
# Test: lab open fails when experiment open
# ============================================
echo ""
echo "--- Testing: lab open fails when already open ---"

assert_failure "open fails when experiment already open" lab open another

# ============================================
# Test: lab snapshot
# ============================================
echo ""
echo "--- Testing: lab snapshot ---"

# Make a code change
echo "modified" >> README.md
git add README.md
git commit -m "Modify readme"

lab snapshot

assert_file_contains "meta has multiple snapshots" "$EXP_DIR/meta.yaml" "snapshots:"
assert_output_contains "snapshot shows commit" "Commit" lab status

# ============================================
# Test: lab add
# ============================================
echo ""
echo "--- Testing: lab add ---"

echo "extra artifact" > extra.txt
lab add extra.txt

assert_file_exists "artifact copied" "$EXP_DIR/artifacts/extra.txt"
assert_file_contains "meta lists artifact" "$EXP_DIR/meta.yaml" "extra.txt"

# ============================================
# Test: lab edit
# ============================================
echo ""
echo "--- Testing: lab edit ---"

# Use true as editor (no-op)
EDITOR=true lab edit

assert_success "edit completes" true

# ============================================
# Test: lab close
# ============================================
echo ""
echo "--- Testing: lab close ---"

lab close

assert_file_empty "current is empty" ".lab/current"
assert_file_contains "meta has status closed" "$EXP_DIR/meta.yaml" "status: closed"

# ============================================
# Test: lab list
# ============================================
echo ""
echo "--- Testing: lab list ---"

assert_output_contains "list shows experiment" "baseline" lab list
assert_output_contains "list shows closed" "closed" lab list

# ============================================
# Test: second experiment
# ============================================
echo ""
echo "--- Testing: second experiment ---"

EDITOR=true lab open test-two

# Should have a new experiment
assert_output_contains "status shows test-two" "test-two" lab status

lab close

# List should show both
OUTPUT=$(lab list)
echo "$OUTPUT" | grep -q "baseline" || { echo "FAIL: list missing baseline"; TESTS_FAILED=$((TESTS_FAILED + 1)); }
echo "$OUTPUT" | grep -q "test-two" || { echo "FAIL: list missing test-two"; TESTS_FAILED=$((TESTS_FAILED + 1)); }

# ============================================
# Test: lab repo has commits
# ============================================
echo ""
echo "--- Testing: lab repo commits ---"

cd "$LAB_DIR"
COMMIT_COUNT=$(git log --oneline | wc -l)
if [ "$COMMIT_COUNT" -gt 0 ]; then
    echo -e "${GREEN}PASS${NC}: lab repo has commits ($COMMIT_COUNT)"
    TESTS_PASSED=$((TESTS_PASSED + 1))
else
    echo -e "${RED}FAIL${NC}: lab repo has no commits"
    TESTS_FAILED=$((TESTS_FAILED + 1))
fi
TESTS_RUN=$((TESTS_RUN + 1))

# Print summary
print_summary
