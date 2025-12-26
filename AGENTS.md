# Agent Instructions

This project uses **bd** (beads) for issue tracking. Run `bd onboard` to get started.

## Quick Reference

```bash
bd ready              # Find available work
bd show <id>          # View issue details
bd update <id> --status in_progress  # Claim work
bd close <id>         # Complete work
bd sync               # Sync with git
```

## Landing the Plane (Session Completion)

**When ending a work session**, you MUST complete ALL steps below. Work is NOT complete until `git push` succeeds.

**MANDATORY WORKFLOW:**

1. **File issues for remaining work** - Create issues for anything that needs follow-up
2. **Run quality gates** (if code changed) - Tests, linters, builds
3. **Update issue status** - Close finished work, update in-progress items
4. **PUSH TO REMOTE** - This is MANDATORY:
   ```bash
   git pull --rebase
   bd sync
   git push
   git status  # MUST show "up to date with origin"
   ```
5. **Clean up** - Clear stashes, prune remote branches
6. **Verify** - All changes committed AND pushed
7. **Hand off** - Provide context for next session

**CRITICAL RULES:**
- Work is NOT complete until `git push` succeeds
- NEVER stop before pushing - that leaves work stranded locally
- NEVER say "ready to push when you are" - YOU must push
- If push fails, resolve and retry until it succeeds

## Testing Protocol

### Integration Tests Are Primary
**ALWAYS add integration tests instead of manually running compiled binaries.**

When implementing new features:

1. **Create test files in `tests/files/`**
   - Add `.c` test files for the new feature
   - Examples: `do_while_basic.c`, `forward_declaration.c`, `mutual_recursion.c`

2. **Run the test suite**
   ```bash
   cargo test
   ```

3. **Accept new snapshots**
   ```bash
   cargo insta accept
   ```

4. **Verify all tests pass**
   ```bash
   cargo test
   ```

### What NOT to do:
❌ **DO NOT** manually compile and run test files like:
```bash
./target/debug/cc2 tests/files/test.c -o /tmp/test && /tmp/test
```

### Why?
- The test harness automatically compiles, runs, and verifies all `.c` files in `tests/files/`
- Snapshots are automatically generated and compared for:
  - Tokenization (`tests/tokenize.rs`)
  - Parsing/AST (`tests/parse.rs`)
  - Code generation (`tests/codegen.rs`)
- This ensures consistent, repeatable testing
- Manual testing doesn't verify against expected output

### Test Coverage
Each feature should have tests for:
- Basic functionality
- Edge cases
- Integration with existing features
- Error conditions (if applicable)

