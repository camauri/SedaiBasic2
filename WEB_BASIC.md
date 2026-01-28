# SedaiBasic Web (sbw.exe)

**Version:** 1.0
**Status:** In Development

## Overview

SedaiBasic Web (`sbw.exe`) is a stand-alone HTTP server that executes Web BASIC scripts (`.wbas` files) in response to HTTP requests, similar to PHP or classic ASP.

### Key Features

- **Stand-alone HTTP server** - No nginx/Apache dependency
- **No SDL2/Graphics/Audio** - Lightweight, server-focused
- **Security by default** - GET$/POST$ are HTML-escaped automatically
- **Simple URL mapping** - `/path/script.wbas` -> `{basedir}/path/script.wbas`

## Quick Start

```bash
# Start the server
sbw --port 8080 --basedir C:\www\scripts

# Test with curl
curl http://localhost:8080/hello.wbas
curl "http://localhost:8080/hello.wbas?nome=Mario"
```

## Command Line Options

| Option | Description | Default |
|--------|-------------|---------|
| `--port <number>` | HTTP port | 8080 |
| `--host <address>` | Bind address | 0.0.0.0 |
| `--basedir <path>` | Script directory | (required) |
| `--cache` | Enable bytecode caching | disabled |
| `--verbose` | Verbose logging | disabled |
| `--help` | Show help | - |

## File Extensions

| Extension | Description | Executable |
|-----------|-------------|------------|
| `.bas` | Standard BASIC (console) | sb.exe, sbv.exe |
| `.wbas` | Web BASIC | sbw.exe |
| `.basc` | Compiled bytecode | sb.exe, sbv.exe |

## Web BASIC Instructions

### Input Functions (with automatic sanitization)

| Function | Description | Security |
|----------|-------------|----------|
| `GET$("name")` | Query string parameter | HTML-escaped (safe) |
| `POST$("name")` | Form POST parameter | HTML-escaped (safe) |
| `GETRAW$("name")` | Query string raw | Not sanitized |
| `POSTRAW$("name")` | Form POST raw | Not sanitized |

**Security principle:** `GET$` and `POST$` return HTML-escaped values by default.

```basic
10 REM Input: nome=Mario<script>alert(1)</script>
20 PRINT GET$("nome")
30 REM Output: Mario&lt;script&gt;alert(1)&lt;/script&gt;
```

For raw data (JSON processing, internal use):
```basic
10 REM Only when necessary and with awareness
20 JSON$ = GETRAW$("data")
```

### Encoding Functions

| Function | Description | Use |
|----------|-------------|-----|
| `HTML$(s)` | HTML escape | `< > & " '` -> entities |
| `URL$(s)` | URL encode | For building URLs |

### HTTP Environment Variables

| Variable | Description | Example |
|----------|-------------|---------|
| `METHOD$` | HTTP method | "GET", "POST" |
| `PATH$` | Requested path | "/hello.wbas" |
| `QUERY$` | Full query string | "nome=Mario&eta=30" |
| `HEADER$("name")` | HTTP request header | `HEADER$("User-Agent")` |

### Response Control

| Instruction | Description | Example |
|-------------|-------------|---------|
| `SETHEADER name, value` | Set response header | `SETHEADER "Content-Type", "application/json"` |
| `STATUS code` | Set HTTP status | `STATUS 404` |

## Example Scripts

### hello.wbas - Simple HTML Page

```basic
10 SETHEADER "Content-Type", "text/html; charset=utf-8"
20 PRINT "<html><body>"
30 PRINT "<h1>Welcome!</h1>"
40 IF GET$("nome") <> "" THEN PRINT "<p>Hello "; GET$("nome"); "!</p>"
50 PRINT "<p>Method: "; METHOD$; "</p>"
60 PRINT "<p>Time: "; TIME$; "</p>"
70 PRINT "</body></html>"
```

**Request:** `GET /hello.wbas?nome=Mario`

**Response:**
```html
<html><body>
<h1>Welcome!</h1>
<p>Hello Mario!</p>
<p>Method: GET</p>
<p>Time: 14:35:22</p>
</body></html>
```

### api.wbas - JSON API

```basic
10 SETHEADER "Content-Type", "application/json"
20 ID$ = GETRAW$("id")
30 IF ID$ = "" THEN ID$ = "0"
40 PRINT "{""id"": "; ID$; ", ""status"": ""ok"", ""time"": """; TIME$; """}"
```

### form.wbas - Form Handling

```basic
10 SETHEADER "Content-Type", "text/html; charset=utf-8"
20 PRINT "<html><body>"
30 IF METHOD$ = "POST" THEN GOTO 100
40 REM Show form
50 PRINT "<form method='post'>"
60 PRINT "<input name='nome' placeholder='Name'>"
70 PRINT "<button>Submit</button>"
80 PRINT "</form>"
90 GOTO 200
100 REM Process POST
110 PRINT "<h1>Hello "; POST$("nome"); "!</h1>"
120 PRINT "<a href='/form.wbas'>Back</a>"
200 PRINT "</body></html>"
```

## Excluded Instructions

The following instructions are **not available** in sbw.exe and will cause syntax errors:

### Graphics
`GRAPHIC`, `DRAW`, `CIRCLE`, `BOX`, `COLOR`, `SCNCLR`, `PLOT`, `LINE`, `PAINT`, `CHAR`, `SPRITE`, `MOVSPR`, `SPRCOLOR`, etc.

### Audio
`SOUND`, `PLAY`, `VOL`, `ENVELOPE`, `TEMPO`, `FILTER`, etc.

### Interactive Console
`INPUT` (blocking), `GET`, `GETKEY`, `INKEY$`

### PEEK/POKE
Only "safe" memory operations. Video/audio memory mapping will cause errors.

## Security Considerations

### XSS Prevention

Use `GET$`/`POST$` (not `GETRAW$`/`POSTRAW$`) when outputting to HTML:

```basic
10 REM SAFE - HTML-escaped
20 PRINT "<p>Hello "; GET$("name"); "</p>"

30 REM UNSAFE - XSS vulnerable
40 PRINT "<p>Hello "; GETRAW$("name"); "</p>"
```

### Path Traversal Protection

The server sanitizes paths and prevents directory traversal:
- `../../../etc/passwd` is blocked
- Only `.wbas` files can be executed
- Files must be within `--basedir`

### Null Byte Injection

Null bytes in paths are rejected.

## Build

```powershell
# Build sbw only
.\build.ps1 -Target sbw

# Build all targets including sbw
.\build.ps1 -Target all

# Build with debug
.\build.ps1 -Target sbw -Debug
```

## Architecture

```
+-------------------------------------------------------------+
|                      sbw.exe                                |
+-------------------------------------------------------------+
|  HTTP Server (fphttpserver)                                 |
|    GET /hello.wbas  ->  executes basedir/hello.wbas         |
|    GET /admin/login.wbas  ->  executes basedir/admin/login  |
+-------------------------------------------------------------+
|  Web I/O Adapter                                            |
|    TWebOutput : IOutputDevice  (PRINT -> response body)     |
|    TWebContext : request/response data                      |
+-------------------------------------------------------------+
|  SedaiBasic Core (NO SDL2, NO Audio, NO Graphics)           |
|    Lexer -> Parser -> SSA -> Bytecode -> VM                 |
+-------------------------------------------------------------+
```

## Comparison with Other Executables

| Executable | Description | SDL2 | Audio | Graphics | Web |
|------------|-------------|------|-------|----------|-----|
| `sb.exe` | Console | No | Optional | No | No |
| `sbv.exe` | Vision (graphic) | Yes | Yes | Yes | No |
| `sbc.exe` | Compiler | No | No | No | No |
| `sbd.exe` | Disassembler | No | No | No | No |
| `sbw.exe` | Web Server | No | No | No | Yes |

## Future Improvements

- Multi-threaded/worker pool mode
- Session management
- Database connectivity
- Template engine integration
- WebSocket support
