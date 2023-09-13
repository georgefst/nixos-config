let
  clark = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBxRb7WoSVlSXAcj6x/uTz6MSO5ZyOOY+yo7wecCI3A2";
  x = { publicKeys = [ clark ]; };
in
{
  "wifi.TNCAPA620AF.psk.age" = x;
  "wifi.RML-5ghz.psk.age" = x;
  "passwords.lta.age" = x;
  "mailgun.sandbox.age" = x;
  "mailgun.key.age" = x;
}
