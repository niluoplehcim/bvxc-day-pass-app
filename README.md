Overview

https://019b2621-4e77-3206-b803-421600641f45.share.connect.posit.cloud

BVXC Payment App is a single-file R Shiny web application that lets a small ski/nordic club sell common products online—day passes, Christmas passes, season passes, programs, special events, and donations—with an integrated cart → pay workflow.

What it does (in one sentence)

Users select items, add them to a cart, and pay using Square Online Checkout; the club’s admins manage prices, availability, limits, events, and reporting inside the app, with settings stored persistently in the database.

Core features

Public purchase flow

Multiple sales categories (day / Christmas / season / programs / events / donation)

Cart with editable quantities (donation amount editable)

Redirect to Square hosted checkout

Return receipt screen with status banner and QR code (QR appears after payment is confirmed)

Admin console (in-app)

Update prices without code changes

Enable/disable tabs (turn product categories on/off)

Set early-bird cutoff and global transaction limits

Manage blocked dates (prevent day-pass payments on specific dates)

Create/manage special events (and optionally enforce capacity)

Run reports and export CSV

How it’s hosted and persists data

Runs as a Shiny app on Posit Connect Cloud (or another Shiny host).

Persists settings and transactions in a database:

Supabase Postgres for deployed use (recommended and effectively required for production)

Optional SQLite fallback for local development only (explicitly guarded by an environment flag)

High-level architecture (plain English)

Shiny renders the UI and manages user sessions (including the in-memory cart).

Database (Postgres/Supabase) stores:

Admin configuration (prices, toggles, limits, etc.)

Transactions and cart contents (for receipts and reporting)

Blocked dates and special events

Event/program sold counts (via a tx_items table) for capacity enforcement

Square provides the payment page and processes cards.

Receipt verification is handled by:

Redirect back to the app using a receipt token, then

Polling Square’s order/payment endpoints to confirm final status (works, but webhooks are the production-grade upgrade).



Public User Guide (Buyers)
1) Open the app

Use the club-provided URL (mobile or desktop).

You will see tabs such as Day Pass, Christmas, Season Pass, Programs, Special Events, Donation (tabs can be enabled/disabled by the club).

2) Select what you want to buy

Choose the relevant tab.

Pick the product type (e.g., adult/youth, family, etc.).

If the item requires a date (typical for day passes), select the date.

If the club has blocked that date, the app will prevent checkout for that day.

3) Add items to the cart

Click Add to cart.

Repeat for additional items (you can mix categories).

4) Review and edit the cart

Open the cart panel (usually visible on the right or below).

You can:

Increase/decrease quantities

Remove items

Adjust donation amount (if donations are enabled)

5) Proceed to payment (Square Checkout)

Click Pay / Checkout.

You will be redirected to Square’s secure hosted payment page.

Enter card details and complete payment.

6) Return to receipt screen (critical)

After payment, Square redirects you back to the app:

The app shows a receipt page with your transaction status.

The QR code (if used by the club for validation) should appear only after the app confirms payment.

If you close the browser too early:

Re-open the app and use the receipt link (if you saved it), or ask the club to look up the transaction.

7) What the QR code means

The QR code represents a receipt token associated with your transaction.

Club staff can scan it to validate the purchase.

If the status is not confirmed (pending/failed), the QR code should not be treated as valid.



Admin User Guide (Club Staff)
Access

Go to the Admin tab.

Enter the admin password (set via environment variable on the host).

Typical tasks
A) Turn tabs on/off (sell or hide categories)

Use the Admin controls to:

Enable/disable Day Pass / Season Pass / Programs / Special Events / Donation tabs

Immediately affects what the public sees

B) Update prices and products

Edit price tables (e.g., Adult day pass, Youth day pass, Family pass, etc.)

Save changes

Changes persist in the database (no redeploy required)

C) Block dates (no day-pass sales)

Add dates to Blocked Dates

Useful for:

Closed trails

Grooming shutdown

Special events where day passes should be restricted

D) Create and manage special events

Define event name, date(s), price, and optional capacity

Enable/disable the event listing

If capacity is enforced, once sold out the app should block further purchases

E) View transactions and export reports

Use Admin reporting to list:

Recent transactions

Status (completed/pending/failed)

Item breakdown (what was sold)

Export CSV for bookkeeping and reconciliation



Troubleshooting (Operators)
Buyer says: “I paid but I didn’t get the QR code”

Most common causes:

They closed the browser before returning to the receipt page

Payment is still pending/processing

Temporary Square/API connectivity issue

What to do:

Ask them to open the receipt page again (if they have it)

In Admin, locate the transaction and verify status

If needed, confirm in Square dashboard using order/payment ID

Buyer says: “My card was charged twice”

Check Admin transactions: do you see 2 completed payments?

Confirm in Square dashboard.

If truly duplicated, refund one payment through Square (don’t try to “fix” it only in the app).

App is slow on first load

Hosting cold-start issue (workers spin up).

Fix: keep at least one worker warm (host-specific setting), reduce heavy startup work, avoid unnecessary DB queries at init.



Deployment & Configuration (Posit Connect / Shiny + Supabase + Square)
1) Architecture (what talks to what)

User browser → Shiny app (hosted on Posit Connect / Connect Cloud)

Shiny app → Supabase Postgres (persistence: config, prices, blocked dates, transactions, registrations)

Shiny app → Square APIs (create checkout/payment, verify status)

Square hosted checkout → redirects back to your app (receipt/QR page)

2) Square configuration (must be correct or payments/redirects break)

In Square Developer Dashboard:

Select the correct environment: Sandbox vs Production

Confirm Location ID matches where you want payments recorded

Add/confirm allowed redirect/return URL(s) that point back to your app’s receipt endpoint

Operational rule:

Your app should only show “paid / QR valid” after it confirms payment status from Square (not merely because the user returned).

3) Supabase database configuration (recommended for persistence)

Use Supabase Postgres instead of local SQLite if you want settings to survive redeploys and multiple workers reliably.

Ensure your DB user has the minimum permissions needed (read/write to the app’s tables).

Use connection pooling / sensible limits (especially if you scale workers).

4) Posit Connect runtime (starting point)

Connect Cloud exposes settings like:

Max worker processes

Max connections

Worker load factor

Startup timeout / Read timeout / Idle timeout / Connection timeout

Starting point (adjust to traffic):

Max worker processes: 2–4

Max connections: 10–30

Worker load factor: 0.5–1.0

Startup timeout: 60–120s (cold start headroom)

Idle timeout: 10–30 min (keeps sessions alive during checkout/receipt flows)

If cold starts hurt user experience, the practical fix is keeping at least one worker warm (Connect/hosting dependent), plus reducing heavy work during app startup.

Environment Variables Checklist

Set these in the hosting platform’s “Environment Variables” / “Secrets” (not hard-coded in the repo).

Square

SQUARE_ENV

values typically: sandbox or production

SQUARE_ACCESS_TOKEN

SQUARE_LOCATION_ID

App security / admin

BVXC_ADMIN_PASSWORD

strong password; rotate periodically

Redirect / receipt verification

BVXC_RETURN_BASE_URL

the public base URL of your deployed app (used to build receipt/return URLs)

Database (Supabase/Postgres)

Common patterns (your app may use one or more; pick the one your code expects):

DB_URL (preferred if your code supports a single connection URL)

example shape: postgresql://user:password@host:5432/dbname

Or separate vars:

DB_HOST, DB_PORT, DB_NAME, DB_USER, DB_PASSWORD

Optional operational flags (if you add them)

LOG_LEVEL (e.g., info, debug)

APP_ENV (e.g., prod, dev)

Minimum safety rules

Never commit tokens/passwords to Git.

Rotate Square tokens if you suspect exposure.

Use different Square credentials for sandbox vs production.

Operational Checklists
Pre-season (once per year)

Update prices and products

Season passes, day passes, programs, special events

Verify tax handling (if applicable) is consistent

Update date logic

Early-bird cutoff date(s) (if used)

Blocked dates for known closures

End-to-end test

Run at least one Sandbox purchase from cart → Square → return → receipt

Confirm receipt page only marks valid after Square confirmation

Confirm admin reports show the transaction correctly

Backups

Export/backup key tables (transactions, registrations, config, blocked_dates, price tables)

Security

Rotate BVXC_ADMIN_PASSWORD

Confirm admin tab is not visible without login (and does not leak data)

Before an event day / holiday rush

Capacity + availability

Set event capacity and verify “sold out” behavior

Add blocked dates where day passes should be disabled

Operational readiness

Confirm Square dashboard access for at least 2 admins (avoid single point of failure)

Confirm the deployed URL matches BVXC_RETURN_BASE_URL

Quick live test: $1 test item (then refund) if your process allows it

During operations (day-to-day)

Monitor:

error logs (timeouts, DB connection errors, Square API errors)

payment completion rate vs “abandoned at checkout”

Have a simple playbook for staff:

“No QR code? Check transaction status in Admin / Square.”



Post-season

Export reports

Total revenue by category

Attendance / day-pass counts by date

Donations summary

Archive

Snapshot config/price tables used that season (for auditability)

Backup database exports to offline storage

Clean up

Remove expired events

Rotate admin password and (optionally) Square tokens



Staff Quick Reference (Front Desk / Trailhead / Volunteer Hut)
What a valid purchase looks like

A purchase is valid only if one of the following is true:

The customer shows a receipt/QR page that says “PAID / VALID” (or equivalent wording your app uses), and it displays the correct item(s), date, and name/email if collected.

You can find the transaction in the Admin → Transactions list and it shows Paid/Completed (not “Pending”, “Created”, “Open”, “Failed”, “Canceled”).

Do not accept “I got charged” screenshots from a bank app as proof. Those can be pending/voided.

When someone says “I paid but I don’t have the QR / it won’t load”
Step 1 — Ask for one of these identifiers (fastest first)

Email used at checkout (if you collect it)

Last 4 digits of card + approximate time

Receipt/order number from Square

Full name + what they bought + approximate time

Step 2 — Check Admin → Transactions

Search by whatever identifier you have:

If you find it and it shows Paid/Completed:

Accept it.

If QR won’t load, you can still let them ski and ask them to refresh later.

If it shows Pending/Open/Created:

They have not completed checkout.

Ask them to reopen the payment link and finish.

If it shows Failed/Canceled:

They did not pay successfully.

They must retry payment.

Step 3 — If Admin is down or slow

Use Square dashboard (phone works):

Find the payment/order by time window and amount.

Confirm status is Completed.

If you cannot confirm completion in either system, treat as unpaid.

When the customer is stuck at checkout (Square page)

Common causes:

Weak cell coverage / Wi-Fi issues

Return redirect blocked by browser or captive portal

They closed the tab before completion

What to do:

Have them switch network (club Wi-Fi ↔ cellular).

Ask them to open the checkout link again and complete payment.

If they completed payment but can’t return, you should still see it in Admin / Square as Completed.

Refunds and mistakes (wrong pass, double purchase)

Rule:

Refunds should be done in Square dashboard (clean, auditable, familiar).

Process:

Find the payment in Square.

Refund the correct amount (full/partial as needed).

If the pass was already used, apply club policy (partial refund or no refund).

Record-keeping:

If your app supports it, add a brief admin note for why it was refunded.

Chargebacks / disputes

Respond using Square’s dispute process.

Your strongest evidence is:

transaction record + timestamp

QR scan/validation record (if you store it)

club policy text (posted publicly)

If you do not store scans, consider adding it next season (it materially helps).

Offline contingency (no internet)

Be blunt: online-only payment systems fail without internet. You need a fallback.

Pick one:

Cash box (simple, reliable)

Manual IOU sheet (name + phone + amount + “pay later” link)

Offline vouchers (pre-sold passes; staff validates from a printed list)

If internet is out:

Sell nothing you can’t record or validate later, unless club leadership has explicitly approved IOU policy.

What staff should never do

Never accept “pending” bank notifications as proof of payment.

Never manually mark someone as paid unless you can see a Completed transaction in Admin or Square.

Never share admin password or leave admin tab logged in on a public tablet.



Coder / Maintainer Guide (BVXC Day Pass App)
1) What this app does (architecture)

Frontend: R Shiny UI (tabs for passes/programs/events/donations/admin).

Payments: Square checkout (customer pays on Square-hosted page).

Return flow: Square redirects back to RETURN_BASE_URL with a token/identifier.

Persistence: Stores transactions + optional config/blocked dates in a database:

Local/dev: typically SQLite file(s)

Production: either SQLite (on server filesystem) or Postgres/Supabase (preferred for persistence and admin edits)

Core principle: Only treat a purchase as valid when Square reports it as COMPLETED/PAID.

2) Repo layout (typical)

app.R — main Shiny app (UI + server + helpers)

renv.lock — pinned R package versions

/.Renviron (local only, not committed) — secrets + environment configuration

rsconnect/ — deployment metadata (Posit Connect / shinyapps.io)

*.sqlite — local DB files (should be gitignored unless intentionally included)

If your repo differs, adjust these headings accordingly.

3) Required environment variables

Set these in Posit Connect / shinyapps.io environment settings (NOT in code).

Square

SQUARE_ENV = sandbox or production

SQUARE_ACCESS_TOKEN = Square API token

SQUARE_LOCATION_ID = Square location id

App

RETURN_BASE_URL = public URL Square redirects to (must match Square config)

BVXC_ADMIN_PASSWORD = password for admin tab

Database (choose one)
Option A — SQLite (simple, weaker persistence in some hosts)

DB_BACKEND = sqlite

SQLITE_PATH = path to sqlite file (example: bvxc.sqlite)

Option B — Postgres/Supabase (recommended)

DB_BACKEND = postgres

DB_URL = full Postgres connection string

Non-negotiable: do not hardcode secrets in app.R.

4) Local development setup (Mac/Linux)
4.1 Install prerequisites

R (recent)

RStudio (optional but recommended)

Git

4.2 Restore dependencies with renv

From repo root:

install.packages("renv")
renv::restore()

4.3 Create .Renviron locally (do not commit)

Example:

SQUARE_ENV="sandbox"
SQUARE_ACCESS_TOKEN="..."
SQUARE_LOCATION_ID="..."
RETURN_BASE_URL="http://127.0.0.1:xxxx"   # local only; for prod use actual URL
BVXC_ADMIN_PASSWORD="..."

DB_BACKEND="sqlite"
SQLITE_PATH="bvxc.sqlite"

4.4 Run locally

In R:

shiny::runApp("app.R", launch.browser = TRUE)

5) Database: schema + lifecycle

The app typically needs tables like:

transactions — what user attempted and what Square confirmed

config — price lists, toggles, season settings, etc.

blocked_dates — dates admin can block sales

registrations / donation_details — if applicable

5.1 Initialization

On startup the app should:

connect (pool preferred)

create missing tables if not present

run migrations if schema changed

5.2 Migrations (how to evolve schema safely)

If you change schema:

Add a schema_version record in config (or dedicated table).

On startup, run stepwise migrations v1 -> v2 -> v3.

Keep migrations idempotent (safe to re-run).

If you don’t already have this, implement it before the schema grows further.

6) Payment flow (Square)
6.1 Create checkout / payment link

App calls Square API to create a checkout/payment session.

You must store at least:

internal receipt_token (your own UUID)

Square order_id and/or payment_id

amount, item type, purchaser info

status: CREATED, PENDING, COMPLETED, FAILED, CANCELED

6.2 Return handling (critical)

When Square redirects back:

App must query Square API to confirm the real status.

Only mark transaction valid if Square says it is COMPLETED (or equivalent final state).

Do not rely solely on query parameters from the browser redirect.

6.3 Webhooks (optional but best practice)

If you implement Square webhooks:

Webhook updates transaction state even if redirect fails.

Requires a public endpoint and signature verification.

If you don’t want webhooks, you must make the redirect handler robust and re-check Square by token.

7) Admin tab (security + behavior)
7.1 Authentication

Admin access should be gated by BVXC_ADMIN_PASSWORD.

Store only a session flag (not the password) in reactive state.

7.2 Admin powers

Typical admin features:

View transactions (filter by status/date/type)

View receipt details by token

Block/unblock dates

Edit config/prices/events

7.3 Hard rule

Admin UI must not allow “manual mark as paid” unless it also verifies Square completed status (or is clearly labeled as an exception with audit trail).

8) Performance and reliability expectations
8.1 Cold start

Minimize work at startup

Use connection pooling

Avoid scanning large tables on load

8.2 Hot path

The receipt/QR page must be fast:

Look up by token using indexed column

Cache config in-memory (TTL 30–300s, not 2s)

One Square verification call max; cache verified results

8.3 Logging

Add structured logs for:

startup timings

Square API timings

DB query timings

errors with context (token/order_id)

This is what you use when someone says “it’s slow” or “QR didn’t load”.

9) Deployment
9.1 Posit Connect / shinyapps.io

Ensure env vars set in the platform UI

Ensure DB persistence is appropriate:

If SQLite on ephemeral filesystem → data loss risk

If Postgres/Supabase → preferred

9.2 Release process

Use git tags for versions: v3.2.1, v3.2.2

Include APP_VERSION string in UI

Keep a CHANGELOG.md

9.3 Rollback strategy

Prior release bundle must be redeployable

DB migrations must be reversible or forward-only with backups

10) Debugging playbook
10.1 “Payment succeeded but app shows invalid”

Likely causes:

return handler didn’t verify Square

token mismatch

Square API call failed (timeout)

caching stale status

Steps:

Find transaction by token in DB

Check stored order_id/payment_id

Query Square dashboard directly

Compare status; update logic accordingly

10.2 “QR/receipt page is slow”

Steps:

Check logs for timing: DB vs Square

Ensure receipt lookup uses indexed token

Ensure Square verification call is cached and not repeated per refresh

10.3 “Admin edits don’t persist”

SQLite on non-persistent host

wrong DB_URL in prod

schema not committed / migrations missing

11) Coding conventions (keep it maintainable)

Recommended structure inside app.R:

GLOBAL SETTINGS

DB helpers

Square helpers

Business logic (pricing rules, early bird cutoff, etc.)

UI

Server

Admin server modules

If the file is getting big, split into:

R/db.R

R/square.R

R/admin.R

R/ui.R
and source them from app.R.

12) What to change safely vs carefully
Safe changes

Text/UI labels

Adding new products/events if driven by config

Admin filtering/reporting

High-risk changes (test thoroughly)

Payment/return flow

Receipt validation logic

DB schema

Auth/admin security

Any changes that affect “is this paid?”

13) Minimal test checklist (before every deploy)

Sandbox purchase succeeds and receipt validates

Cancelled payment shows invalid

Refresh receipt page: stays valid, doesn’t re-hit Square repeatedly

Admin login works, transactions list loads

Blocked date prevents purchase

App starts cleanly from cold start
