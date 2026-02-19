# Page 5: How to Obtain the API Key and Form ID (CHEF)

If you are using the **Common Hosted Forms Service (CHEF)** for the BC Public
Service, you can find both the API key and the Form ID directly in the FormIO
management interface.

## API Key (CHEF)

1. Open your form in CHEF.
2. Click **Manage** in the form menu.
3. In the Manage page, generate or copy your **API Key**.

![CHEF API Key location](../assets/API.png)

## Form ID (CHEF)

1. Open your form in CHEF.
2. Click **Manage** in the form menu.
3. Look at the URL in your browser. The **Form ID** is the final alphanumeric
   code after the `=` sign.

![CHEF Form ID location](../assets/FormID.png)

## Other FormIO services

FormIOr is designed for CHEF and uses the default base URL:

```
https://submit.digital.gov.bc.ca/app/api/v1
```

Note (CHEF): Metadata/schema endpoints may live at `/api/v1`. FormIOr will
automatically retry the alternate base for metadata/schema requests when needed.

If you are using a different FormIO service, you must **override the base URL**
when calling `GetResponses()` or `GetSubmissions()`. This is not guaranteed to
work if the service differs from CHEF.

---

Navigation
- Home: [Handbook Index](../index.md)
- Prev: [Security and Credentials](04-security-credentials.md)
- Next: [Standard Workflow](../workflows/05-standard-workflow.md)
