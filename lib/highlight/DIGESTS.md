## Subresource Integrity

If you are loading Highlight.js via CDN you may wish to use [Subresource Integrity](https://developer.mozilla.org/en-US/docs/Web/Security/Subresource_Integrity) to guarantee that you are using a legimitate build of the library.

To do this you simply need to add the `integrity` attribute for each JavaScript file you download via CDN. These digests are used by the browser to confirm the files downloaded have not been modified.

```html
<script
  src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/highlight.min.js"
  integrity="sha384-5xdYoZ0Lt6Jw8GFfRP91J0jaOVUq7DGI1J5wIyNi0D+eHVdfUwHR4gW6kPsw489E"></script>
<!-- including any other grammars you might need to load -->
<script
  src="//cdnjs.cloudflare.com/ajax/libs/highlight.js/11.11.1/languages/go.min.js"
  integrity="sha384-HdearVH8cyfzwBIQOjL/6dSEmZxQ5rJRezN7spps8E7iu+R6utS8c2ab0AgBNFfH"></script>
```

The full list of digests for every file can be found below.

### Digests

```
sha384-GSGd4G9IDg7LPIIpfZuqfVsqOxYND9rvc1BKkv4xJMPe7RWeLauvfOga8hJ2Cw00 /es/languages/lisp.js
sha384-kB1QfFMBwbQxnyNzZiWt6/rlLEODkTklgd4NfbEHc/8TYL6oV83o+Yydz8ZIx3Ie /es/languages/lisp.min.js
sha384-Cmq5lORXzyHraasLNqmfchH07JRXyEmjDF+j6tSggoXjYHwtgX/ySW6kkRytM5uu /es/languages/python.js
sha384-ZV5sgX70bBgLkDR5Mtox5UsbJedBc39hRKPdvTw6miK4lSkE/wv94cLY2iyZb/sB /es/languages/python.min.js
sha384-HZV/dhDa0Ue0fPBAGMmwuiv9vQZaT2jcez0/IXCbLe1MCSp9TwA6tvoaUhlw+kuW /languages/lisp.js
sha384-XHf/F3AFtpwh/qvji8cbUFV7wLfrPKcyItaFlzg2/tS8EEBRgjCzWagzMOF4A6TP /languages/lisp.min.js
sha384-ueSSFZFqg7cVD0dpEqIk9EefJiJUYan0PH6I8u/p+bNLLx7dMs4J2keMaFXqCN8P /languages/python.js
sha384-eXRt+aAa2ig1yFVDQCLis8k9s/1dikTcigj+/R07yNdIxc8BAG/b1uHDyEW3of17 /languages/python.min.js
sha384-7EdWNOMmXlC/OI2K8nf2ipa6Op0V+BgjOHMLYlcYpwUa6WFet06kSNKEIR7bl80j /highlight.js
sha384-55vstZwlRN8iu8F7QEfJnwg/AdktkGesJtNnLdmUbkHDKm+lXRSHF7Br/QY5DST4 /highlight.min.js
```

