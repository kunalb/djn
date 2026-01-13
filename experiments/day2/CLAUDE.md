# Claude Code Guidelines for Self-Contained Web Tools

## Philosophy

Build **simple, self-contained, powerful tools** as single HTML files. No build steps, no dependencies, no frameworks. Just open the file in a browser and it works.

## Core Principles

### 1. Single File Architecture
- Everything in one `.html` file: markup, styles, scripts
- Inline CSS in `<style>` tags
- Inline JS in `<script>` tags
- No external dependencies (fonts can use system fallbacks)

### 2. Data First
Design the data structure before the UI:
- Use localStorage for persistence
- Structure data as plain JSON that's easy to export/import as text
- Make data human-readable when exported (for querying, backup, portability)
- Example pattern:
```javascript
{
  "2024-01-15": {
    items: [{ id, timestamp, ...fields }],
    notes: "free text"
  }
}
```

### 3. Minimal UI, Maximum Function
- Monospace fonts (Berkeley Mono, SF Mono, Consolas fallbacks)
- Neutral colors (cosmic latte, warm greys, subtle accents)
- Small, crisp elements (thin borders, minimal padding)
- Show information density over whitespace
- No emojis in UI unless explicitly requested

### 4. Interaction Patterns
- Click to create
- Drag to move/resize
- Double-click for secondary actions
- Contenteditable for inline editing
- Hover to reveal delete/actions
- Keyboard shortcuts where sensible

### 5. Robustness
- Save on every change (debounce if needed)
- Handle edge cases (midnight crossing, empty states)
- Escape user content (prevent XSS)
- Track event listeners to prevent duplicates
- Persist state across refreshes

### 6. PWA Support
Add for offline capability:
```html
<meta name="theme-color" content="#f5f0e1">
<meta name="apple-mobile-web-app-capable" content="yes">
<link rel="manifest" href="data:application/json,...">
```
And inline service worker via blob URL.

### 7. Import/Export
Always include:
- Export as plain text (human-readable format)
- Import from that same format
- Data portability is essential

## Code Structure Template

```html
<!DOCTYPE html>
<html lang="en">
<head>
  <meta charset="UTF-8">
  <meta name="viewport" content="width=device-width, initial-scale=1.0">
  <!-- PWA meta tags -->
  <title>Tool Name</title>
  <style>
    /* Reset */
    * { box-sizing: border-box; margin: 0; padding: 0; }

    /* Base styles */
    body { font-family: monospace stack; }

    /* Component styles */
  </style>
</head>
<body>
  <!-- Minimal HTML structure -->

  <script>
    // ============ STATE ============
    const STORAGE_KEY = 'tool_data';
    let state = { ... };

    // ============ DATA ============
    function load() { ... }
    function save() { ... }

    // ============ RENDERING ============
    function render() { ... }

    // ============ INTERACTIONS ============
    // Event handlers, drag/drop, etc.

    // ============ IMPORT/EXPORT ============
    function exportData() { ... }
    function importData() { ... }

    // ============ INIT ============
    function init() { ... }
    init();
  </script>
</body>
</html>
```

## Style Guidelines

### Colors (Cosmic Latte Theme)
```css
--bg-primary: #FFF8E7;      /* cosmic latte */
--bg-secondary: #f5f0e1;    /* warm grey */
--bg-card: #fff;            /* white */
--border: #e0d9c8;          /* subtle border */
--text: #3a3a3a;            /* dark grey */
--text-muted: #999;         /* muted */
--accent: #7a9ec2;          /* blue */
--accent-alt: #a8b5a0;      /* green */
--danger: #c9462c;          /* red */
```

### Typography
```css
font-family: 'Berkeley Mono', 'SF Mono', 'Fira Code', 'JetBrains Mono', Consolas, monospace;
font-size: 11-13px for content;
font-size: 9-10px for labels;
```

### Spacing
- Padding: 4-8px for elements
- Gaps: 8-12px between items
- Border-radius: 2-3px (subtle)

## Common Patterns

### Drag and Drop
```javascript
let dragState = null;

element.addEventListener('mousedown', startDrag);
document.addEventListener('mousemove', doDrag);
document.addEventListener('mouseup', endDrag);

// Clean up listeners in endDrag
```

### Prevent Duplicate Listeners
```javascript
const setupDone = new WeakSet();
function setup(el) {
  if (setupDone.has(el)) return;
  setupDone.add(el);
  // Add listeners
}
```

### Safe Text Insertion
```javascript
// Don't do: el.innerHTML = `<div>${userText}</div>`
// Do:
el.innerHTML = `<div class="text"></div>`;
el.querySelector('.text').textContent = userText;
```

### LocalStorage with Fallback
```javascript
function load() {
  try {
    return JSON.parse(localStorage.getItem(KEY)) || {};
  } catch {
    return {};
  }
}
```

## Checklist Before Done

- [ ] Works offline (PWA)
- [ ] Data persists across refresh
- [ ] Export produces readable text
- [ ] Import restores from export
- [ ] No XSS vulnerabilities
- [ ] Edge cases handled (empty state, boundaries)
- [ ] No duplicate event listeners
- [ ] Scrolls sync if multiple panels
- [ ] Current time/state visible
- [ ] Delete actions available
- [ ] Responsive to window size
