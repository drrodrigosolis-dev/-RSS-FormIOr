## Test environments
* Local macOS Tahoe 26.3, R 4.5.2 (aarch64-apple-darwin20)
  - `R CMD build .`
  - `R CMD check --as-cran FormIOr_1.0.2.tar.gz`
* R-hub (GitHub Actions matrix)
  - `windows (R-devel)`, `linux (R-devel)`, `macos-arm64 (R-devel)`
  - all matrix jobs completed successfully for run `subpentagonal-hammerheadbird`
  - <https://github.com/drrodrigosolis-dev/RSS-FormIOr/actions/runs/22195108632>
* win-builder
  - `devtools::check_win_devel('.')` completed (R-devel)
  - <https://win-builder.r-project.org/Kv983JvSO1O8>
  - Status: `1 NOTE` on R Under development (2026-02-18 r89435 ucrt)

## Resubmission changes (CRAN feedback addressed)
1. **Quoted software/API names in DESCRIPTION**
   - Updated Title/Description to use single quotes for software/API names: `'FormIO'`, `'FormIO API'`, `'Excel'`.
2. **Added API web reference in DESCRIPTION**
   - Added `<https://apidocs.form.io>` in Description with angle brackets and valid `https:` format.
3. **Removed interactive wrappers from examples**
   - Replaced interactive example guards with `\dontrun{}` in documentation where needed (including `assign_section_hierarchy`).
4. **Console output made suppressible / interactive-only**
   - `AskCredentials()` now explicitly requires interactive sessions.
   - `rename_columns_from_dictionary()` and `resolve_duplicate_values()` now support non-interactive, suppressible workflows (`rename_map`, `strategies`, `prompt`, `quiet`).
   - Interactive prompts remain only in interactive code paths.
5. **No default writes to home / working directory**
   - Export/audit helpers require explicit file paths.
   - Function examples use `tempfile()`/`tempdir()` patterns.
   - Handbook and cheat-sheet examples updated to write under `tempdir()`.

## Additional cleanup for CRAN readiness
* Reworked credential/session handling to use internal package state (`.formior_state`) instead of global assignment.
* Updated the RStudio addin to use a local `user_env <- globalenv()` reference (instead of literal `.GlobalEnv`) to avoid global-assignment check notes.
* Regenerated all roxygen docs (`devtools::document()`) and refreshed handbook/cheat-sheet artifacts.
* Updated `.Rbuildignore` to exclude non-package artifacts (`.github`, `handbook_site`, local check artifacts, etc.).

## R CMD check results
`0 ERROR | 0 WARNING | 1 NOTE`

NOTEs:
* New submission
