---
name: wtf-quiz-me
description: >-
  Quiz the user on a subject, one question at a time, starting broad and
  drilling down based on how they answer, so they can find out what they
  actually understand and rebuild what they have lost. Takes a PR, a branch or
  ref, a concept in the current codebase ("how the render system works"), or
  any topic at all ("2022 F1 drivers", "IFR in Microsoft Flight Simulator
  2024"). Use when the user asks to be quizzed or tested: "quiz me on", "test
  my understanding of", "do I actually understand this PR", "/wtf-quiz-me".
  NOT for explaining something (just answer), or for writing a quiz, flashcards
  or exam for someone else to take.
argument-hint: '[PR, branch, ref, concept in this repo, or any topic]'
allowed-tools: Read, Grep, Glob, Agent, AskUserQuestion, WebSearch, WebFetch, Bash(~/.claude/scripts/resolve-scope.sh:*), Bash(gh pr view:*), Bash(gh pr diff:*), Bash(git show:*), Bash(git log:*), Bash(git diff:*)
---

# Quiz me

Arguments: $ARGUMENTS

A lot of code now gets written faster than anyone reads it, and the person who
merged it can lose track of how their own system works. This skill is for
finding out, honestly, what the user still knows. The user does the talking.
You ask, you grade, you correct briefly, and you pick the next question based
on the answer.

## 1. Read the subject before asking anything

Work out what the arguments name, then read it until you could explain it
yourself. If they are empty, ask the user what they want to be quizzed on.
Don't default to the current branch.

**A PR, branch, ref or path in this repo.** Run
`~/.claude/scripts/resolve-scope.sh resolve --scope "<arguments>"` and read the
`scope.diff` and `manifest.json` it points to.
`~/.claude/reference/scope-resolution.md` explains the output. Exit `2` means
the arguments are prose, so go to the next case. For a PR, also read its title
and body (`gh pr view <n> --json title,body`), because *why* the change was made
is worth asking about. Then read the code around the change as well as the diff.
A quiz on the diff alone only tests what is already on the screen. The questions
that matter are how the change fits into the rest of the system. When the
manifest's `correspondence` says the working tree does not hold the scope, read
files with `git show "<scope_head>:<path>"` so you are not reading different
code.

**A concept in this codebase.** Find the entry points with Grep and Glob, then
follow the main path through the code. If the area is too big to read directly,
send an `Explore` agent to map it first, then read the files it names. You need
the understanding in this conversation, not just a summary of it.

**Anything else.** Use what you know, but check it first wherever you could be
wrong: anything after your training cutoff, anything tied to a version or
release (a 2024 sim, a 2022 season), niche details, exact numbers, dates and
names. Confirm those with WebSearch or WebFetch before they go into a question.

When the words could mean either a concept in this repo or a general topic
("the scheduler"), ask which one the user means.

## 2. Build an answer key and keep it to yourself

Split the subject into a few threads. For a PR, those might be the problem it
solves, what it changes, what it puts at risk, and what the tests do and don't
cover. Give each thread three levels:

1. **Orientation.** What is it, what is it for, and why does it exist?
2. **Mechanism.** How does it work? Which parts are involved, and in what order?
3. **Depth.** Edge cases, trade-offs, failure modes, what would break if it
   changed, and why this design won over the obvious alternative.

**Only ask what you can grade.** Every fact in the key must come from something
you read in this session or something you are certain of. If you are not sure
of an answer, drop the question. A wrong answer key is worse than a missing
question, because grading the user against it teaches them something false.

## 3. Ask

Open with one line naming the subject and the controls: `hint`, `skip`,
`harder`, `easier`, `stop`. Then ask the first question.

- **One question per turn, then stop and wait.** Never ask several at once.
  Keep each question short enough to read on a phone.
- **Start at orientation** and let the answers decide where to go next:
  - a solid answer: go one level deeper on the same thread
  - a partial answer: say which part was right, then ask a follow-up about the
    part that was missing
  - a wrong answer or "don't know": give the answer in two or three sentences,
    citing `file:line`, the PR section or the source it came from. Then move to
    an easier question or a different thread. Come back to the same idea later
    with a question phrased differently.
  - two misses in a row on one thread: move on to a new thread instead of
    pushing harder on that one
- **Ask what someone would need to know to review, debug or change the code.**
  Don't ask trivia they could look up in two seconds, like line numbers,
  exact variable names or argument order. Useful kinds of question:
  - *explain why*: the best kind; ask this most often
  - *predict*: "what happens if this is null", or "what happens with the flag off"
  - *locate*: "where would you change X", which tests whether they can find
    their way around
  - *spot it*: show a short snippet with one thing changed or broken, and ask
    what is wrong
- **Multiple choice suits recall topics** like drivers, frequencies or
  procedures. Use AskUserQuestion with plausible wrong options. Don't put
  anything in an option's description that gives the answer away, and never
  mark an option as recommended.
- **Don't give away later answers.** A question shouldn't contain its own
  answer, and a correction shouldn't answer the next question.
- **Grade honestly.** Accept paraphrases and different reasoning that is still
  correct. Say "partly" when an answer is partly right. Don't praise more than
  the answer earns. If the user disagrees with the key, check the source again:
  the code or the source settles it, not the key. They may know something you
  missed.

About every eight questions, offer to keep going or stop.

## 4. Wrap up

When the user stops, or you run out of threads, give a short summary:

- **Solid**: what they clearly know
- **Shaky**: what they got partly right
- **Gaps**: what they missed, each with a pointer to read next (`file:line`, the
  PR section, or a source URL)

Nothing gets written to disk. The quiz happens in the conversation and ends there.
