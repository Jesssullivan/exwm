# IPC Encoding Format Research: Wayland Compositor <-> Emacs Bridge

## Use Case Requirements

- **Primary**: ~100 msg/s for window management events (focus, geometry, workspace changes)
- **Future**: ~200 msg/s for eye-tracking data relay
- **Transport**: Unix domain socket (local only, no network)
- **Constraint**: Emacs is single-threaded; parsing must not block the UI event loop

---

## 1. S-expressions via `lexpr` (Rust) / `read`/`print` (Emacs Lisp)

### Rust Side: `serde-lexpr` / `lexpr`

| Metric | Value |
|---|---|
| Crate | `serde-lexpr` 0.1.3, `lexpr` 0.2.7 |
| Last release | March 2023 |
| Downloads | ~135K total, ~19K/month |
| Stars | 196 (lexpr-rs repo) |
| Open issues | 8 |
| License | MIT / Apache-2.0 |

**Assessment**: Niche crate, low download count (~700x less than serde_json). The API
mirrors serde_json closely, so integration is straightforward. Supports Emacs Lisp
dialect specifically. Maintenance is infrequent but the codebase is small (~6.5K LOC)
and stable. No known correctness issues.

**Performance**: No published benchmarks. As a text format with serde, expect
performance in the same ballpark as serde_json (within 2x). S-expression parsing is
simpler grammar than JSON (no escaping rules for keys, no colon/comma delimiters),
but the lexpr implementation is less optimized than serde_json's SIMD-accelerated
parser.

**Estimated encode/decode**: ~3-6 us for a typical window-event message (~200 bytes).

### Emacs Side: `read` / `prin1-to-string`

**Maturity**: `read` is a C-level primitive in Emacs, battle-tested for 40+ years.
It is the foundation of all Elisp evaluation. `prin1-to-string` is equally fundamental.

**Performance**: Emacs `read` parses S-expressions natively in C. Benchmark data from
[Kisaragi Hiu (2025)](https://kisaragi-hiu.com/2025-11-10-read-or-json) shows
`json-parse-buffer` is ~2.87x faster than `read` on equivalent data. This means
`read` is slower than the native JSON parser, but still fast enough: roughly
~1.9s for 100K iterations on a large payload, which works out to ~19 us/call for
a moderately-sized message.

For small IPC messages (~200 bytes), `read` likely completes in ~2-5 us.

**Key advantage**: Zero dependency on Emacs side. No packages to install. The format
is the native data representation of Emacs Lisp.

### Message Size

S-expressions are comparable to JSON for typical payloads. Slightly more compact for
simple cases (no mandatory quoting of keys, no colons/commas), slightly less compact
for deeply nested structures (more parentheses). Estimated overhead: **~90-110% of
JSON size** depending on payload shape.

Example:
```
;; S-expression (82 bytes)
(window-focus :id 42 :title "Firefox" :app-id "firefox" :workspace 2 :x 100 :y 50)

;; JSON equivalent (101 bytes)
{"type":"window-focus","id":42,"title":"Firefox","app_id":"firefox","workspace":2,"x":100,"y":50}
```

### Risks

- `serde-lexpr` is a one-maintainer project with infrequent updates
- If the maintainer abandons it, forking is feasible (small codebase)
- No ecosystem momentum; few other projects use it

---

## 2. MessagePack via `rmp-serde` (Rust) / `msgpack.el` (Emacs)

### Rust Side: `rmp-serde`

| Metric | Value |
|---|---|
| Crate | `rmp-serde` 1.3.1 |
| Last release | December 2025 |
| Downloads | ~63M total, ~10.7M/month |
| Stars | 1.4K (msgpack-rust repo) |
| Open issues | 67 |
| License | MIT |

**Assessment**: Well-established crate, widely used in production. The rmp family
provides both low-level (`rmp`) and high-level serde (`rmp-serde`) APIs. Supports
zero-copy deserialization. Active maintenance.

**Performance** (from [djkoloski/rust_serialization_benchmark](https://github.com/djkoloski/rust_serialization_benchmark)):

| Operation | serde_json | rmp-serde | rmp-serde advantage |
|---|---|---|---|
| Serialize | 3.57 ms (batch) | 1.52 ms (batch) | **2.3x faster** |
| Deserialize | 6.09 ms (batch) | 3.12 ms (batch) | **2.0x faster** |

Per-message for a small IPC payload (~200 bytes): estimated ~0.5-1.5 us encode,
~1-3 us decode.

### Emacs Side: `msgpack.el`

| Metric | Value |
|---|---|
| Package | `msgpack.el` (xuchunyang/msgpack.el) |
| Stars | 10 |
| Open issues | 0 |
| Language | Pure Emacs Lisp |

**Assessment**: This is the critical weakness. `msgpack.el` is a pure-Elisp
implementation with minimal adoption (10 stars, 3 forks). It is NOT in GNU ELPA or
MELPA's core packages. Being pure Elisp, it cannot match the performance of Emacs's
native C parsers.

**Performance**: No published benchmarks. Pure-Elisp binary parsing is inherently
slow compared to C-level `read` or `json-parse-string`. Expect **5-20x slower** than
native JSON parsing. For a 200-byte message, estimated ~50-200 us per decode -- still
feasible for 100 msg/s (total ~5-20 ms/s of CPU), but leaves little headroom for
200 msg/s eye-tracking data.

**Alternative**: Write a C dynamic module for Emacs that wraps a msgpack C library
(e.g., msgpack-c). This would match or beat JSON performance but requires maintaining
a compiled Emacs module -- significant complexity.

### Message Size

MessagePack is a binary format. Typical size is **~57-67% of JSON** for the same data.

Example (approximate):
```
;; JSON: 101 bytes
{"type":"window-focus","id":42,"title":"Firefox","app_id":"firefox","workspace":2,"x":100,"y":50}

;; MessagePack: ~68 bytes (estimated)
;; Binary encoding: fixmap(7) + fixstr keys + values
```

### Risks

- Emacs-side library is immature and slow (pure Elisp)
- Binary format is not human-debuggable without tooling
- Would need to write/maintain a C module for acceptable Emacs-side performance
- Adds a hard dependency on a third-party Emacs package

---

## 3. JSON via `serde_json` (Rust) / `json.el` (Emacs)

### Rust Side: `serde_json`

| Metric | Value |
|---|---|
| Crate | `serde_json` 1.0.149 |
| Last release | 2025 (continuously updated) |
| Downloads | ~717M total |
| Stars | 4.8K+ |
| Open issues | Actively triaged |
| License | MIT / Apache-2.0 |

**Assessment**: The gold standard of Rust serialization. Extremely well maintained by
David Tolnay. SIMD-accelerated parsing available. Battle-tested in every major Rust
project. Zero risk of abandonment.

**Performance** (from [rust_serialization_benchmark](https://github.com/djkoloski/rust_serialization_benchmark)):

Per-message for a small IPC payload (~200 bytes): estimated ~1-3 us encode,
~2-5 us decode.

### Emacs Side: `json-parse-string` / `json-serialize` (built-in since Emacs 27)

**Maturity**: Native C implementation using libjansson, shipped with Emacs since
version 27.1 (2020). This is a **built-in** with zero external dependencies.

**Performance**:
- `json-parse-buffer` is **2.87x faster** than Elisp `read` and **17x faster** than
  pure-Elisp `json-read` ([benchmark source](https://kisaragi-hiu.com/2025-11-10-read-or-json))
- For 100K iterations on a medium payload: ~666 ms total = **~6.7 us/call**
- For small IPC messages (~200 bytes): estimated **~1-3 us/call**
- Emacs 30 introduced an even faster JSON parser that outperforms the Emacs 27
  jansson-based parser

**Real-world validation**: lsp-mode processes hundreds of JSON messages per second
using `json-parse-string`. The [emacs-lsp-booster](https://github.com/blahgeek/emacs-lsp-booster)
project demonstrated that JSON parsing overhead in Emacs is manageable -- their 4x
improvement came from converting JSON to elisp bytecode, but the baseline JSON
parsing was already adequate for LSP workloads.

### Message Size

JSON is the largest of the three text formats for numeric-heavy data, but competitive
for string-heavy data. Typical window management messages are mixed.

**Overhead vs MessagePack**: ~1.5-2.3x larger (from benchmark: 1,827,461 vs 784,997
bytes on HTTP log dataset).

For our use case with small messages (~100-200 bytes), the absolute difference is
~30-50 extra bytes per message. At 200 msg/s, that is ~6-10 KB/s additional
throughput over a Unix socket -- negligible.

### Risks

- Slightly larger messages than alternatives (irrelevant for Unix socket IPC)
- JSON does not natively support symbols/keywords (must be encoded as strings)
- No risk of library abandonment on either side

---

## Comparison Summary

| Criterion | S-expressions | MessagePack | JSON |
|---|---|---|---|
| **Rust crate quality** | Niche (135K dl) | Strong (63M dl) | Best-in-class (717M dl) |
| **Rust encode speed** | ~3-6 us | ~0.5-1.5 us | ~1-3 us |
| **Rust decode speed** | ~3-6 us | ~1-3 us | ~2-5 us |
| **Emacs-side maturity** | Built-in (`read`) | Poor (`msgpack.el`) | Built-in (`json-parse-string`) |
| **Emacs decode speed** | ~2-5 us | ~50-200 us (pure Elisp) | ~1-3 us |
| **Message size (200B payload)** | ~80-100 bytes | ~65-75 bytes | ~100-120 bytes |
| **Human debuggable** | Yes | No | Yes |
| **External deps (Emacs)** | None | msgpack.el | None |
| **External deps (Rust)** | serde-lexpr | rmp-serde | serde_json |
| **100 msg/s budget** | ~0.5 ms/s | ~5-20 ms/s (Emacs bottleneck) | ~0.3 ms/s |
| **200 msg/s budget** | ~1.0 ms/s | ~10-40 ms/s (Emacs bottleneck) | ~0.6 ms/s |

---

## Niri IPC Architecture Analysis

[Niri](https://github.com/YaLTeR/niri) is a scrollable-tiling Wayland compositor
with a well-designed IPC system worth studying.

### Protocol Design

- **Transport**: Unix domain socket at `$NIRI_SOCKET`
- **Format**: Newline-delimited JSON (NDJSON)
- **Framing**: Each message is a single line of JSON terminated by `\n`
- **Crate**: [`niri-ipc`](https://crates.io/crates/niri-ipc) v25.11.0

### Request/Reply Model

```
Client                          Compositor
  |---- Request (JSON + \n) ---->|
  |<--- Reply (JSON + \n) ------|
  |---- Request (JSON + \n) ---->|
  |<--- Reply (JSON + \n) ------|
```

- Requests are processed sequentially, one at a time
- Each `Request` gets exactly one `Reply`
- `Reply` is defined as `Result<Response, String>` -- wraps success or error

### Event Stream Model

```
Client                          Compositor
  |-- Request::EventStream --->|
  |<-- Reply::Ok(Handled) -----|
  |<-- Event (JSON + \n) ------|   (full initial state)
  |<-- Event (JSON + \n) ------|   (incremental updates)
  |<-- Event (JSON + \n) ------|   (continuous until close)
```

- After sending `Request::EventStream`, the compositor stops reading requests
- First events deliver **complete current state** (e.g., all workspaces, all windows)
- Subsequent events are **incremental updates** (window opened, focus changed, etc.)
- State can never desync: the initial dump + updates form a complete picture
- Connection remains open until the client closes it

### Type Inventory

**Request variants** (15 total):
`Version`, `Outputs`, `Workspaces`, `Windows`, `Layers`, `KeyboardLayouts`,
`FocusedOutput`, `FocusedWindow`, `PickWindow`, `PickColor`, `Action(Action)`,
`Output { output, action }`, `EventStream`, `ReturnError`, `OverviewState`

**Event variants** (11 total):
`WorkspacesChanged`, `WorkspaceUrgencyChanged`, `WorkspaceActivated`,
`WindowsChanged`, `WindowOpenedOrChanged`, `WindowClosed`, `WindowFocusChanged`,
`WindowUrgencyChanged`, `KeyboardLayoutsChanged`, `KeyboardLayoutSwitched`,
`OverviewOpenedOrClosed`

### Key Design Decisions Worth Adopting

1. **NDJSON framing**: Simple, debuggable, zero-overhead framing. No length prefixes
   or delimiters beyond newline. Works naturally with line-oriented tooling (`jq`,
   `socat`, process filters).

2. **Initial state dump on event stream**: Eliminates race conditions. Client never
   needs to separately query state and then subscribe -- the subscription itself
   delivers current state.

3. **Typed request/response enums**: Serde tagged enums serialize cleanly to JSON.
   Type safety on both sides of the socket.

4. **Shared IPC crate**: The `niri-ipc` crate is published separately so third-party
   tools can depend on it without pulling in the full compositor.

5. **One-directional event stream**: After subscribing, the socket becomes write-only
   from the compositor's perspective. This simplifies the protocol and avoids
   bidirectional multiplexing complexity.

### Limitations to Improve On

- No support for multiple concurrent subscriptions with filters
- No message ID / correlation for concurrent request pipelining
- The IPC crate version tracks the compositor version (not semver-stable)
- No explicit keepalive or heartbeat mechanism

---

## Recommendation

### Primary choice: JSON (`serde_json` + `json-parse-string`)

**Rationale**:

1. **Emacs-side performance is the bottleneck**, and JSON has the fastest Emacs-side
   parser by a wide margin (native C, 2.87x faster than `read`, 17x faster than
   pure-Elisp alternatives). At 200 msg/s, JSON parsing consumes ~0.6 ms/s of Emacs
   time. This is negligible.

2. **Zero dependencies on both sides**. `serde_json` is a de facto standard in Rust.
   `json-parse-string` is built into Emacs 27+. No third-party packages to track.

3. **Debuggability**. Messages can be inspected with `jq`, `socat`, `cat`. This
   matters enormously during development of a new compositor.

4. **Proven at scale**. lsp-mode demonstrates that Emacs can handle high-throughput
   JSON IPC without issues. Niri uses the same approach in production.

5. **Message size is irrelevant**. The ~30-50 byte overhead per message vs MessagePack
   is meaningless over a Unix socket. At 200 msg/s with 150-byte messages, total
   throughput is ~30 KB/s. A Unix socket can handle hundreds of MB/s.

### Architecture: Follow niri's NDJSON + event stream pattern

- Newline-delimited JSON over Unix socket
- Request/reply for commands (focus window, move workspace, etc.)
- Event stream for state synchronization (window changes, focus changes, etc.)
- Initial state dump on subscription to prevent desync
- Shared types crate between compositor and Emacs bridge

### When to reconsider

- **S-expressions**: If the Emacs-side code becomes heavily Lisp-idiomatic and
  converting between JSON objects and Elisp structures creates friction. The `read`
  function is fast enough. The risk is serde-lexpr maintenance.

- **MessagePack**: Only if message size becomes a real constraint (unlikely for IPC)
  or if a native Emacs msgpack module is written. Not recommended as primary format.

- **Hybrid approach**: Use JSON for the primary IPC channel. If eye-tracking data
  at 200 msg/s proves too chatty, consider a separate binary channel (msgpack or
  raw structs) for that specific high-frequency stream, decoded in a helper process
  rather than in Emacs directly.

---

## Sources

- [lexpr-rs GitHub](https://github.com/rotty/lexpr-rs)
- [serde-lexpr on lib.rs](https://lib.rs/crates/serde-lexpr)
- [msgpack-rust GitHub](https://github.com/3Hren/msgpack-rust)
- [msgpack.el GitHub](https://github.com/xuchunyang/msgpack.el)
- [serde_json crate](https://crates.io/crates/serde_json)
- [Rust serialization benchmarks](https://github.com/djkoloski/rust_serialization_benchmark)
- [MsgPack vs JSON benchmark (Rust)](https://aeshirey.github.io/code/2020/11/25/benchmarkng-msgpack-vs-json-in-rust.html)
- [Emacs read vs json-parse benchmark](https://kisaragi-hiu.com/2025-11-10-read-or-json)
- [emacs-lsp-booster](https://github.com/blahgeek/emacs-lsp-booster)
- [lsp-mode performance guide](https://emacs-lsp.github.io/lsp-mode/page/performance/)
- [niri IPC wiki](https://github.com/YaLTeR/niri/wiki/IPC)
- [niri-ipc crate docs](https://docs.rs/niri-ipc/latest/niri_ipc/)
- [niri-ipc Request enum](https://docs.rs/niri-ipc/latest/niri_ipc/enum.Request.html)
- [niri event stream discussion](https://github.com/YaLTeR/niri/discussions/411)
- [MessagePack vs JSON size comparison](https://msgpack.org/index.html)
