# Lean Crafter

A web-playable Crafter game built entirely in **Lean 4**, served via the [Lithe](https://github.com/joshpurtell/lithe) web framework.

## Overview

- **CrafterLean** (`src_lean4/`) — Pure Lean 4 implementation of the Crafter game engine
- **CrafterWeb** (`lithe/`) — Lithe-based web app with HTTP API + WebSocket streaming
- **Rust shim** (`lithe/shim/`) — Axum-based HTTP server that bridges requests into Lean

## Quick Start (local)

### Prerequisites

- [elan](https://github.com/leanprover/elan) (Lean version manager)
- Rust toolchain
- The [Lithe](https://github.com/joshpurtell/lithe) repository cloned as a sibling directory

### Build & Run

```bash
# Clone lithe as sibling (if not already)
cd /path/to/parent
git clone https://github.com/JoshuaPurtell/lithe.git

# Build Lean code
cd lean-crafter/lithe
lake build

# Run the server
cd shim
cargo run
```

Open `http://127.0.0.1:3000` in your browser.

### Environment Variables

- `PORT` or `LITHE_BIND` — Bind address (default: `127.0.0.1:3000`)
- `LITHE_APP` — App name (default: `crafter`)

## Deploy to Railway

This repo includes a `Dockerfile` for Railway deployment.

1. Push this repo to GitHub
2. Create a new Railway project from this repo
3. Railway will auto-detect the Dockerfile and build

The app listens on `$PORT` (set automatically by Railway).

## Project Structure

```
lean-crafter/
├── src_lean4/           # CrafterLean — game engine in Lean 4
│   ├── CrafterLean/     # Core modules (world, entities, actions, etc.)
│   └── lakefile.toml
├── lithe/               # CrafterWeb — Lithe web application
│   ├── CrafterWeb.lean  # Routes, handlers, WebSocket
│   ├── shim/            # Rust HTTP server
│   └── lakefile.lean
└── README.md
```

## API

### Create session
```
POST /api/sessions
Body: { "seed": 123 }
Response: { "id": "abc123" }
```

### WebSocket stream
```
WS /api/sessions/:id/stream

// Client → Server
{ "type": "action", "action": "MoveUp" }

// Server → Client
{ "type": "frame", "frame": "..." }
```

### Controls
- Arrow keys / WASD — Move
- Space — Interact (Do action)

## License

MIT
