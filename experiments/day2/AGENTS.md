# Agent Guidelines for Building Self-Contained Tools

## Project Type
Single-file HTML tools with localStorage persistence. No frameworks, no build tools, no external dependencies.

## Workflow

### Phase 1: Planning
1. Understand the core use case
2. Design data structure FIRST (JSON that exports to plain text)
3. Sketch UI layout (columns, panels, interactions)
4. Identify key interactions (create, edit, delete, drag, etc.)
5. Get user approval before coding

### Phase 2: Implementation
Build in this order:
1. HTML structure (semantic, minimal)
2. CSS styles (inline, cosmic latte theme)
3. State management (load/save localStorage)
4. Rendering functions
5. Core interactions (CRUD)
6. Advanced interactions (drag, resize)
7. Import/export
8. PWA support
9. Bug fixes and edge cases

### Phase 3: Refinement
- Commit frequently as snapshots
- Review for bugs after features complete
- Check: XSS, duplicate listeners, edge cases, data loss scenarios

## Interaction Style

### Ask Early
- Data structure preferences
- Layout preferences (sidebar vs footer, etc.)
- Duration/timing defaults

### Commit Often
- After each major feature
- Before making breaking changes
- When user requests a snapshot

### Fix Bugs Systematically
When asked to check for bugs:
1. Read the entire file
2. Look for: XSS, event listener leaks, race conditions, edge cases
3. List findings with severity
4. Fix critical bugs first

## Code Conventions

### JavaScript
- Use `const`/`let`, never `var`
- Clear section comments: `// ============ SECTION ============`
- Functions grouped by purpose
- State at top of script

### CSS
- Mobile-first isn't needed (desktop tool)
- Use ID selectors sparingly (for JS hooks)
- Class-based styling
- Inline in `<style>` tag

### HTML
- Minimal nesting
- Semantic where sensible
- IDs for JS targets
- Classes for styling

## Common User Requests

| Request | Action |
|---------|--------|
| "make it draggable" | Add mousedown/move/up handlers |
| "persist across refresh" | Already using localStorage, check for bugs |
| "make it installable" | Add PWA manifest + service worker |
| "import/export" | Plain text format, symmetrical parse/format |
| "fix the scroll" | Check overflow, use flexbox, sync columns |
| "make it thinner/crisper" | Reduce padding, font-size, border-width |
| "change colors" | Update CSS variables or direct values |
| "check for bugs" | Full file review, list issues, fix critical |

## Don'ts

- Don't add npm/packages
- Don't suggest external CSS frameworks
- Don't create multiple files
- Don't add emojis unless asked
- Don't over-engineer (no classes, no modules, no TypeScript)
- Don't add features not requested
- Don't use innerHTML with user content

## Quality Bar

A tool is done when:
1. Core functionality works
2. Data persists correctly
3. Can export and re-import without loss
4. Works offline (PWA)
5. No critical bugs
6. UI is clean and functional
7. User is satisfied
