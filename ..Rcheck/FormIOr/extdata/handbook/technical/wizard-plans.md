
# Wizard Plans (JSON/RDS)

Wizard plans record the choices you made in a run. They can be reused to
repeat the same steps later, either automatically or as defaults.

## Why plans matter

- Consistency across runs
- Repeatable analysis pipelines
- Less manual work the next time

## Plan formats

- `wizard_plan.json` (human-readable)
- `wizard_plan.rds` (fast to load in R)

## Using a plan

When the wizard starts, it can ask if you want to load a prior plan. You
can apply it immediately or treat it as default answers for each step.

---

Navigation
- Home: [Handbook Index](../index.md)
- Prev: [Plotting Error Handling](plotting-errors.md)
- Next: [Rendering the Handbook](render-handbook.md)
