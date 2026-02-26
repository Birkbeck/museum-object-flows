# MM DB Manager

The Typescript files in this directory define the Google Apps Script functionality for the Google Sheets interface to the Mapping Museums Database.

## Before you first deploy

Changes made locally to the files can be deployed to the Google Sheet using CLASP.

Make sure you have CLASP installed:

```
npm install -g @google/clasp
```

Login to the Google Account which owns the sheet and script:

```
clasp login --user prod
```

If you want to run a testing sheet, you also need to login to the account where the testing sheet is (this can be the same as the account with the production sheet, but does not need to be).

```
clasp login --user test
```

Find the script ID of the Apps Script and clone it locally:

```
clasp clone <script ID>
```

Enable the Apps Scripts API at:

```
https://script.google.com/home/usersettings
```

## Deploying

Check any changes you have made do not break the tests:

```
npm run test
```

Ideally, you should also test the sheet in Google Sheets using a testing sheet:

```
npm run push:test
```

When you are happy with the changes, push the code to the production Google Apps Script:

```
npm run push
```

This will build the Typescript files in `src`, output them into `dist` and then push them to the remote Google Apps Script.