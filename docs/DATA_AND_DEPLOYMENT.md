# Data processing and deployment

## Application behaviour

- Inputs and uploads are processed on the Shiny server; calculation does not
  run exclusively in the browser.
- Data, mappings and results are held in session memory. The app does not create
  a permanent case database. Technical warnings and error messages can contain
  uploaded values and be recorded in server logs, including warnings from file
  readers. Sanitised browser error messages do not prevent server-side logging.
- Recorded temporary upload files are removed when replaced and when a session
  is recognised as ended, where cleanup can run. Cleanup is restricted to files
  whose resolved paths lie within R's temporary directory; it does not
  recursively delete directories.
- Previous calculations can remain in session memory until replaced or the
  session is disposed of. Replacing an upload does not immediately erase every
  in-memory copy. Stale calculations are unavailable for download.
- Generated downloads also use temporary server files. Downloaded copies are
  under the recipient's control. Hosting logs, backups and downloaded copies
  have separate retention and deletion arrangements.
- Display language is session-local. Translations are bundled with the app;
  switching languages does not send data to a translation service or translate
  raw data, change mappings or clear saved calculations.

## Data to process

For hosted use, enter or upload only data that cannot be linked to an individual.
Use only the required fields and, if needed, a row ID. Names, dates of birth and
case reference numbers are not needed. Replacing names with IDs does not by
itself make a dataset anonymous; consider all remaining values and any available
means of linking records. Synthetic examples can be used to try the workflows.
CSV row IDs are read as text to preserve leading zeroes.

## Hosting and privacy information

For Posit Connect Cloud, Posit documents hosting on AWS **us-east-2 (USA)**.
Its hosting service processes technical connection and usage information under
its privacy policy. Its service terms restrict the submission of personal and
sensitive data; the app's use of row IDs does not establish that a particular
dataset is permitted.

- [Connect Cloud security documentation](https://docs.posit.co/connect-cloud/user/platform/security.html)
- [Posit privacy policy](https://posit.co/about/privacy-policy/)
- [Posit service terms](https://posit.co/about/posit-service-terms-of-use/)
- [Connect Cloud data persistence](https://docs.posit.co/connect-cloud/user/platform/system.html#data-persistence)

Connect Cloud describes most runtime files as reset at application startup or
republishing, but notes that `/tmp` files can persist between frequent
republishes. This is not a per-session deletion guarantee or a defined deletion
period for every provider-held copy.

The actual deployment needs its own operator/contact, legal basis, intended
users, access settings and retention arrangements. These details are not inferred
from the repository. Other hosting platforms require information appropriate
to their own setup; the Connect Cloud description does not identify the hosting
provider of every copy of this application.

## Technical operation

- Configure HTTPS and access control for the intended hosting context.
- Uploads are limited to 10 MiB and 100,000 data rows per file. No external
  database or API credentials are required for local calculations.
- Keep original case data out of Git, CI artifacts, issue reports and public
  discussions. Tests and examples contain no user case records.
- Test dependency and application updates before deployment. See
  [Contributing](../CONTRIBUTING.md) for the test and release workflow.
- The retained `rsconnect` metadata describes an existing deployment target.
  Setup and tests do not themselves publish the app. A linked Connect Cloud
  deployment can republish on a Git push if automatic publishing is enabled;
  check that setting before pushing to its configured branch.
- Check the hosted version separately after deployment, including a calculation
  and export in a new session. Local tests do not verify a hosted release.
