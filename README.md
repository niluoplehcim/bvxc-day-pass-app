# BVXC Payment App (Shiny + Square + Supabase)

This is a Shiny web app used by the Bulkley Valley Cross Country Ski Club (BVXC) to sell passes/program registrations, accept donations, and process payments through Square.

The key design goal: **club admins can change settings (prices, dates, events, tabs) without editing code**, and those settings **persist online** when the app is hosted on Posit Connect Cloud by using **Supabase Postgres**.

---

## What users can do

Public tabs:
- **Day Pass**: pick a ski date, add passes to cart.
- **Christmas Pass**: pick a 14‑day window that must include Dec 25, add to cart.
- **Season Pass**: adult / youth passes (early‑bird vs regular pricing supported).
- **Programs**: fixed program list with admin-set prices.
- **Special Events**: choose an event created by Admin, add registrations to cart.
- **Donation**: add a donation to cart (not tax‑deductible unless the club becomes a registered charity).

Cart:
- Review cart items
- Enter name/email for receipt
- **Pay now** redirects to Square hosted checkout
- After successful payment, the app can display a “thank you” panel and a receipt QR code (when redirect is configured)

Admin:
- Password‑protected
- Enable/disable tabs
- Set all prices
- Set early-bird cutoff date
- Set transaction limits
- Block dates (prevent payment on specific dates)
- Create/update/delete special events

---

## Database modes (important)

This app supports two database modes:

### A) Production persistence (recommended)
If `BVXC_DB_URL` is set, the app uses **Postgres (Supabase)**.
- Admin settings persist across redeployments
- Events/blocked dates persist
- Transaction records persist

### B) Local development fallback
If `BVXC_DB_URL` is **not** set, the app uses **SQLite** (`bvxc.sqlite` by default).
- Good for local testing
- Not recommended for production persistence on Connect Cloud

---

## Environment variables (Secrets)

Do **not** hardcode secrets into `app.R`.

### Required (Connect Cloud Secrets)
- `SQUARE_ENV`  
  Must be exactly: `sandbox` or `production`

- `BVXC_SANDBOX_MODE`  
  Must be exactly: `fake` or `square`  
  - `fake` = simulate payment (no Square redirect)  
  - `square` = real Square sandbox hosted checkout

- `SQUARE_ACCESS_TOKEN`  
  Square access token (sandbox or production).

- `SQUARE_LOCATION_ID`  
  Your Square location id.

- `BVXC_ADMIN_PASSWORD`  
  Admin tab password.

- `BVXC_RETURN_BASE_URL`  
  Your deployed Connect Cloud app URL **without** a trailing slash.  
  Example: `https://your-app-name.connect.posit.cloud`

- `BVXC_DB_URL`  
  Supabase Postgres connection string. Use the **pooler** connection method (session pooler / Supavisor) from Supabase.

Recommended libpq keyword/value format:
```
host=aws-1-ca-central-1.pooler.supabase.com port=5432 dbname=postgres user=postgres.<your-project-ref> password=<db-password> sslmode=require
```

Optional:
- `BVXC_DB_PATH` (SQLite filename for local mode; default `bvxc.sqlite`)

---

## Local development (quick start)

1) Go to the repo folder:
```bash
cd ~/Documents/bvxc-day-pass-app
```

2) Create a local `.Renviron` for development (do not commit it):
```bash
SQUARE_ENV=sandbox
BVXC_SANDBOX_MODE=fake
SQUARE_ACCESS_TOKEN=...
SQUARE_LOCATION_ID=...
BVXC_ADMIN_PASSWORD=...
# Leave BVXC_DB_URL empty to use SQLite locally
# BVXC_DB_URL=
```

3) Run the app:
```bash
R -q -e "shiny::runApp('.', launch.browser=TRUE)"
```

---

## Deploy to Posit Connect Cloud (overview)

1) Ensure Connect Cloud can access the GitHub repo.
   - Some plans require the repo to be public.

2) Create content from the repo in Connect Cloud.

3) Add the Secrets listed above.

4) Publish.

---

## Security notes

- **Never** commit `.Renviron` or any file containing real tokens/passwords.
- Use Connect Cloud **Secrets** for production values.
- If a secret was ever pushed to GitHub, rotate it immediately (Square token / Supabase DB password).

---

## Common problems

### “SQUARE_ENV must be one of: sandbox, production”
Your `SQUARE_ENV` secret is missing or not exactly `sandbox` / `production`.

### “SQUARE_ACCESS_TOKEN is not set”
You did not set `SQUARE_ACCESS_TOKEN` in Connect Cloud Secrets (or it’s blank).

### Supabase hostname doesn’t resolve
Use the Supabase **pooler** host (session pooler / Supavisor) instead of the direct `db.<project>.supabase.co` host.

### Square checkout refuses to connect
This can happen if the app is embedded in an iframe. The app includes an iframe-escape redirect handler; confirm you’re testing the published Connect Cloud URL in a normal browser tab.
