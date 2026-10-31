# Data processing and deployment

## Application behaviour

- Single-case inputs and uploads are processed on the Shiny server; calculation
  does not run exclusively in the browser.
- The code does not create a permanent case database or write case details to
  application logs. Import errors are shown without internal server paths.
- Uploads temporarily exist as Shiny files. The app removes its recorded upload
  files when a file is replaced and when a session is recognised as ended.
  Cleanup is restricted to files under R's temporary directory.
- A lost connection may only be recognised as a session end after a timeout.
  A process crash can prevent explicit cleanup. The application therefore does
  not promise immediate or verifiably complete deletion of every copy across
  the hosting infrastructure.
- The browser and server retain session data in memory. Downloaded exports are
  subsequently under the recipient's control.
- Infrastructure logs, backups and their retention periods are outside the
  control of this code.
- The display language is session-local. Switching languages does not translate
  raw data, send data to a translation service, change mappings or clear results.
  The translation catalog is bundled with the application.

Upload only the required fields. Names, dates of birth and case reference
numbers are not needed. A pseudonymous case ID can support row matching;
pseudonymised data is not automatically anonymous.

## Deployment-specific information to complete

The operator of each deployment must document its actual responsibility and
contact details, legal basis, authorised users, hosting region, data-processing
arrangements, any international transfers, and retention/deletion periods.
These details are not inferred from repository metadata. The general application
information does not replace a deployment-specific privacy notice.

According to [Posit's documentation](https://docs.posit.co/shinyapps.io/guide/security_and_compliance/index.html),
shinyapps.io is hosted in AWS us-east-1 (USA). Nonpersistent container storage is
not a guarantee of immediate deletion. Assess suitability and required
arrangements before uploading real data. A self-hosted Shiny or Posit Connect
instance is an alternative.

## Technical operation

- Configure HTTPS and access control to match the deployment context.
- Uploads are limited to 10 MiB and 100,000 rows. No external database or API
  credentials are required.
- Do not put original datasets into Git, CI artifacts, issue reports or public
  discussions. All supplied tests and examples are synthetic.
- Test updates in a separate environment, update `renv.lock` deliberately, run
  reference and browser checks, and increment the app version.
- The existing `rsconnect` file describes the previous deployment target; it does
  not trigger publication. This revision does not publish automatically.
- Both UI languages use the same convictions-only German model implementation.
  A language preference is not a jurisdictional model selector.
