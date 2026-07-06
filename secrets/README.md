# Agenix secrets

## Editing keys

Edit one key:

```sh
agenix -e github.key.age
```

Note that, for consistency, we always use a single trailing newline.

Edit all keys:

```sh
for i in $(nix eval --file ./secrets.nix --json | jq 'keys | .[]' -r)
do
    echo "Editing $i:"
    agenix -e $i
done
```

For `wifi.age`, the file contents need to look like:

```sh
nix eval --file ../nix/wifi.nix --json | jq -r '.[] | "PSK_\(.)=FILL_THIS_IN"'
```

## Re-keying

If we add a new SSH key to our config, we'll need to re-key the `.age` files. Unfortunately, for passphrase-protected keys, `agenix -r` will request the passphrase [for _every_ key](https://github.com/ryantm/agenix/issues/252). Assuming that our user SSH key has a passphrase, but the system one does not, it's easier to do:

```sh
sudo agenix -r -i /etc/ssh/ssh_host_ed25519_key
sudo chown gthomas:gthomas *.age
```

## Getting fresh API tokens

- GitHub
  - https://github.com/settings/tokens
  - Classic API key, with "repo" scope
- Mailgun
  - https://app.mailgun.com/settings/api_security
