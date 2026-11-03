# Data processing and deployment

## Application behaviour

- Single-case inputs and uploads are processed on the Shiny server; calculation
  does not run exclusively in the browser.
- Session data, mappings, results and timestamps are held in memory. The
  application does not create a permanent case database or write case details
  to application logs. Import errors do not expose internal server paths.
- Uploads temporarily exist as Shiny files. The app removes its recorded upload
  files when a file is replaced and when a session is recognised as ended.
  Cleanup is restricted to files whose resolved paths lie within R's temporary
  directory; it does not recursively delete directories.
- A lost connection may only be recognised as a session end after a timeout.
  A process crash can prevent explicit cleanup. Application cleanup therefore
  cannot guarantee immediate or complete deletion across the hosting infrastructure.
- Generated downloads also use temporary server files. Downloaded copies are
  under the recipient's control. Infrastructure logs, backups and retention
  periods are managed by the hosting operator.
- Display language is session-local. Translations are bundled with the app;
  switching languages does not send data to a translation service or translate
  raw data, change mappings or clear saved calculations.

Upload only the required fields. Names, dates of birth and original case
reference numbers are not needed. A pseudonymous case ID can support row
matching; pseudonymised data is not automatically anonymous. CSV IDs are read
as text to preserve leading zeroes. Export fields are described in the README.

## Hosting and privacy information

Each deployment needs its own privacy information: the operator and contact,
legal basis, authorised users, access controls, hosting region, data-processing
arrangements, international transfers where applicable, and retention/deletion
periods. An author link or repository metadata does not establish these details.

The [Posit security and compliance documentation](https://docs.posit.co/shinyapps.io/guide/security_and_compliance/index.html)
describes shinyapps.io hosting in AWS us-east-1 (USA). Review the provider's
current terms and the deployment's requirements before processing real case
data. Self-hosted Shiny or Posit Connect deployments require their own operating
and privacy arrangements.

## Technical operation

- Configure HTTPS and access control for the intended users and hosting context.
- Uploads are limited to 10 MiB and 100,000 data rows per file. No external
  database or API credentials are required for local calculations.
- Keep original case data out of Git, CI artifacts, issue reports and public
  discussions. Tests and examples contain no user case records.
- Test dependency and application updates before deployment. See
  [Contributing](../CONTRIBUTING.md) for the test and release workflow.
- The retained `rsconnect` metadata describes a deployment target. Setup,
  automated tests and Git pushes do not deploy the application.
- Check the hosted version separately after deployment, including a calculation
  and export in a new session. Local test results do not verify a hosted release.
