---
description: Open a page in Chrome via the devtools MCP server and debug it until the error is gone
argument-hint: "[url — e.g. localhost:3000/cart or https://example.com/#/route]"
---

Navigate to $ARGUMENTS with the devtools MCP server. Investigate, debug and
iteratively solve the error. Add any temporary logging you may need to get the
job done.

Read the console and the failing network requests before changing anything —
the stack trace usually names the file.

Keep going until the page works. Reload and confirm the error is actually gone
rather than assuming the fix landed.

Remove the temporary logging when you are done.
