
# Page 9: Wizard Overview

The FormIOr wizard is a guided, interactive workflow that walks you through
end-to-end processing of FormIO responses. It is designed for non-technical
users who want a clear path from download to deliverable outputs.

> **CHEF credentials tip:** If you are using the BC Public Service CHEF FormIO,
> your API key and Form ID are found on the form’s **Manage** page. The Form ID
> is the alphanumeric code after the `=` sign in the URL. See:
> [How to Obtain the API Key and Form ID (CHEF)](../rationale/05-api-formid.md)

## What the wizard does

- Creates an output folder with all generated files
- Starts (or reuses) an audit log that records each action
- Downloads FormIO submissions and flattens the nested structure
- Guides you through cleaning steps and diagnostics
- Produces codebooks, plots, and export files
- Saves a plan file so you can repeat the same steps later

## Why it is useful

- It reduces the risk of forgetting a step
- It makes workflows repeatable across team members
- It provides a paper trail for transparency and reporting

## Typical wizard run

```r
out <- run_form_processing_workflow()
```

You will be asked a sequence of simple questions. You can accept defaults
by pressing Enter, or provide specific values if you need to change them.

---

Navigation
- Home: [Handbook Index](../index.md)
- Prev: [Repeatable Manual Workflows](../workflows/08-repeatable-manual.md)
- Next: [Step 1 - Output Folder and Files](10-step-output-folder.md)
