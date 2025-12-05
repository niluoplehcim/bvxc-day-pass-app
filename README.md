# Bulkley Valley Cross Country Ski Club – Passes App

Shiny app for the Bulkley Valley Cross Country Ski Club to sell:

- **Day passes**
- **Season passes**
- **Christmas 2-week passes**
- **Standalone donations**

Payments are handled via **Square Online Checkout** (hosted payment links).  
The app never sees or stores card details.

---

## 1. Requirements

- R (4.x recommended)
- Packages:
  - `shiny`
  - `httr`
  - `jsonlite`
  - `DBI`
  - `RSQLite`
  - `uuid`

On a new machine, from R:

```r
install.packages(c("shiny", "httr", "jsonlite", "DBI", "RSQLite", "uuid"))
2. Files in this repo
app.R – main Shiny application

.gitignore – excludes local artefacts such as:

.Rhistory, .RData, .Rproj.user

bvxc.sqlite and other *.sqlite

CSV logs (*.csv)

.Renviron (env vars)

Other files (SQLite DB, CSV logs, etc.) are generated at runtime and should not be committed.

3. Configure environment variables
The app uses environment variables for all sensitive values (Square credentials, admin password, etc.).

In the app directory (same folder as app.R), create a file named .Renviron with content like:

bash
Copy code
SQUARE_ENV=sandbox
SQUARE_ACCESS_TOKEN=YOUR_SANDBOX_ACCESS_TOKEN
SQUARE_LOCATION_ID=YOUR_LOCATION_ID
BVXC_ADMIN_PASSWORD=some-strong-password
BVXC_RETURN_BASE_URL=http://127.0.0.1:3838
Notes:

SQUARE_ENV:

sandbox for test / development

production for real payments

SQUARE_ACCESS_TOKEN:

From Square Developer dashboard (Sandbox or Production as appropriate)

SQUARE_LOCATION_ID:

Location ID for the club’s Square location

BVXC_ADMIN_PASSWORD:

Password for accessing the ADMIN tab (reports, exports, config)

BVXC_RETURN_BASE_URL (optional but recommended for production):

For local use: http://127.0.0.1:3838

For hosted use (e.g. shinyapps.io): the public URL of the app

After editing .Renviron, restart R so the variables are picked up.

.Renviron is in .gitignore and must never be committed to GitHub.

4. Run the app locally
From R, with the working directory set to the app folder:

r
Copy code
setwd("~/Documents/bvxc-day-pass-app")  # adjust path as needed
shiny::runApp(".", host = "0.0.0.0", port = 3838)
Then in a browser go to:

http://127.0.0.1:3838

If the environment variables are set correctly and Square sandbox credentials are valid, you will be able to:

Select passes / donations

See live totals

Click Pay to be redirected to a Square checkout page

See a "Payment complete" modal after redirecting back with ?success=1

5. Deployment (e.g. shinyapps.io)
There are two ways to deploy:

5.1 Using rsconnect from R
Install rsconnect if needed:

r
Copy code
install.packages("rsconnect")
Configure your shinyapps.io account (only once per machine):

r
Copy code
rsconnect::setAccountInfo(
  name   = "YOUR_SHINYAPPS_ACCOUNT_NAME",
  token  = "YOUR_TOKEN",
  secret = "YOUR_SECRET"
)
Deploy from the app directory:

r
Copy code
rsconnect::deployApp()
5.2 Using the shinyapps.io web UI
Create a new application in shinyapps.io.

Upload app.R.

Configure environment variables in the shinyapps.io dashboard (see below).

5.3 Environment variables on shinyapps.io
In the shinyapps.io dashboard, for this specific app, set:

SQUARE_ENV

SQUARE_ACCESS_TOKEN

SQUARE_LOCATION_ID

BVXC_ADMIN_PASSWORD

Optional: BVXC_RETURN_BASE_URL (set this to the public URL of the deployed app)

These values must not be hard-coded in app.R and must not be stored in the repo.

Once deployed, the app will:

Use the Square API endpoint appropriate to SQUARE_ENV

Generate checkout links that redirect back to the hosted app URL

6. Data files and logging
The app uses a local SQLite database bvxc.sqlite (in the app directory) with these tables:

transactions

One row per transaction (day / season / Christmas / donation)

registrations

One row per pass holder (season & Christmas)

blocked_dates

Dates where no online day-pass sales are allowed

config

Stored prices and limits (rather than hardcoding)

donation_details

Additional metadata for donations (address, consent flags, notes)

There is also a small CSV:

tab_flags.csv

Stores which public tabs are enabled/disabled (Day, Christmas, Season, Donation).

All of these are runtime data and are excluded via .gitignore.
For production, they should be:

Located in a persistent storage area (depending on hosting), and

Backed up periodically.

7. Security & privacy notes
No card data is handled by this app. All payments use Square’s hosted checkout.

The app stores:

Purchaser name and email

Names and dates of birth for season pass holders

Optional donor details (address, postal code, consent flags, notes)

Admin access is controlled by BVXC_ADMIN_PASSWORD:

Required for all reports, exports, config changes, and blocked-date management.

Environment variables (Square tokens, admin password) must not be:

Committed to Git

Shared in screenshots

Included in any README example with real values

For privacy and legal compliance, the club should:

Limit access to the bvxc.sqlite file and exports.

Use strong passwords for admin and Square accounts.

Periodically review and purge data if required by local regulations.

8. License / usage
This code is intended for internal club use.

No explicit open-source license has been set.
Please contact the Bulkley Valley Cross Country Ski Club committee before:

Re-using this code in other organisations

Deploying modified versions outside the club context

Publishing the app or code publicly

markdown
Copy code

To finish:

1. On GitHub, open `README.md`, click **Edit**.
2. Replace everything with the block above.
3. Click **Commit changes** to `main`.

Done.
