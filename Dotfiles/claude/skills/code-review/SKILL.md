---
name: code-review
description: Review code changes and remove AI-generated patterns like excessive comments, gratuitous defensive checks, type escape hatches, etc.
---

# Code Review Skill

Review code changes and remove AI-generated patterns that don't match human-written code.

## Usage

When asked to review a branch or diff, check for and remove AI code slop.

## What to Look For

### Excessive Comments

AI tends to over-comment. Remove comments that:
- State the obvious (e.g., `// increment counter` above `counter++`)
- Repeat the function/variable name
- Are inconsistent with commenting patterns elsewhere in the file
- Explain *what* instead of *why*

```typescript
// ❌ Remove: States the obvious
// Check if the user is valid
if (isValidUser(user)) {

// ❌ Remove: Repeats the code
// Set the status to active
status = 'active';

// ✅ Keep: Explains why
// Must check expiry before validation because expired tokens cause cryptic errors
if (isExpired(token)) return null;
```

### Type Escape Hatches

AI often casts to `any` to silence type errors. Fix the types instead.

```typescript
// ❌ Bad: Casting to any
const result = (data as any).value;

// ✅ Good: Fix the type
interface DataWithValue { value: string; }
const result = (<DataWithValue>data).value;

// ✅ Also good: Type guard
if (hasValue(data)) {
    const result = data.value;
}
```

### Style Inconsistencies

Check for patterns that differ from the rest of the file:

- Different naming conventions (camelCase vs snake_case)
- Different import styles (namespace vs named)
- Different error handling patterns
- Different comment styles


### Verbose Logging

AI adds excessive logging. Match the codebase's logging level.

```typescript
// ❌ Remove if file doesn't log at this level
console.log('Processing started');
console.log('Validating input...');
console.log('Input validated successfully');
console.log('Processing complete');

// ✅ Keep: Matches existing error logging pattern
console.error(`Failed to process order ${orderId}: ${error.message}`);
```

Feel free to add excessive logging if needed when debugging an issue, but remove it when done.

## Review Process

1. **Get the diff**: Compare against main branch
2. **Scan each file**: Look for the patterns above
3. **Check consistency**: Compare against unchanged portions of same file
4. **Make targeted fixes**: Remove slop without changing correct code
5. **Summarize**: Report 1-3 sentences on what was changed

## Output Format

After reviewing, provide a brief summary:

```
Removed 3 redundant null checks in order-processor.ts (upstream validation handles these). 
Deleted 8 obvious comments and converted 2 unnecessary try/catch blocks to let errors propagate.
```
