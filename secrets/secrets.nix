let
  ci = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILrgvfWpzhkG/0RM2dvpaNEAOG24+XwfSOJHFy2GaI3W root@ci";
  seungheon = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGvBhZ/fpfkncoGuiA3UYQ95qt67ldDzxfhrYtfLmmhB";
in
{
  "hackage-keys.tar.age".publicKeys = [ seungheon ci ];
}
