# k3-racket
k3 language parser in racket

## Installation

1. Install Racket from https://racket-lang.org/
2. Install required dependencies:
   ```bash
   raco pkg install brag-lib beautiful-racket-lib gui-easy-lib
   ```
3. Install this package:
   ```bash
   raco pkg install
   ```

## Running

### Editor
To run the k3 editor with syntax highlighting:
```bash
racket editor.rkt
```

### Parser Only
To parse a k3 file without the GUI:
```bash
racket parse-only.rkt
```

### Main Program
To run the main k3 program:
```bash
racket main.rkt
```

The editor will open with the file specified in `k-path` (currently "example.k") and display it with syntax highlighting using a platform-appropriate monospace font (Consolas on Windows, Menlo on macOS, DejaVu Sans Mono on Linux).
