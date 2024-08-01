let
  clark = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBxRb7WoSVlSXAcj6x/uTz6MSO5ZyOOY+yo7wecCI3A2";
  x = { publicKeys = [ clark ]; };
in
{
  "github.key.age" = x;
  "mailgun.key.age" = x;
  "mailgun.sandbox.age" = x;
  "wifi.age" = x;
}
