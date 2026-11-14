# Lens: ui

**Looks for:** user-facing interface quality, read from the code rather than
rendered: a layout that breaks at phone width (a fixed width, content that
cannot wrap, a row or table that overflows sideways), a list or async view with
no loading, empty or error state, a button with no disabled or pending state
while its action runs, form validation not shown beside its field, a control the
keyboard cannot reach or operate, focus that is not visible, a tap target too
small for touch, one-off styling where the project's own components, tokens or
classes already cover it.

Read the interface from its code; do not render it.

## Evidence bar for one-off styling

It is an assertion about code outside the diff: search first, and name the
existing component or token by `file:line`.
