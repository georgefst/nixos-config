# TODO this file should be used for SSH keys
# incl. `keys = config.users.users.gthomas.openssh.authorizedKeys.keys` for `nix.sshServe`
# plus `nix.sshServe`
# (whole `serve Nix store over SSH` section which needs more general review, ideally from Fable, and stuff to be moved elsewhere)
# and maybe also  even use this file for other non-SSH stuff
{
  fry = {
    ssh.user = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIENRoUCeCrR6KtXi/Trx5igMumHuDR2enXubiisk+QTE";
    ssh.system = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIGigM5uHEdyX7x4GXAYY5YxdYIH/3pt+XlhagfqRVtm+";
    syncthing.id = "FAN563X-T7BSJSJ-QCMJOYM-LO3US4T-HCVC2FT-WIW4RRT-YL4ZFGJ-3LPUKQR";
  };
  crow = {
    ssh.user = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAICqVpc5ttFcpEX4BL19nLmx4Nyl4bLvqfRBMoITUv7A1";
    ssh.system = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILeV081Gv4Gxxqoko//8anSYWITZV7OWL83bZM7eigmt";
    syncthing.id = "R3LMVOB-OEFWL5Q-3NFXHWT-YNHXNAT-WQBB3P4-G5ZHMES-IMQTKM4-6MFWFQI";
  };
  clark = {
    ssh.system = "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIJzXoCO1nw+vYI1rQ5o3BzU0EOSdlR6dFZC/qAlIm9QC";
    syncthing.id = "2DXPUJC-4B6TIZQ-N5ESX5I-52RBOOS-3443BUK-SZVUCYM-MILGZHZ-YNXAAAA";
    syncthing.introducer = true;
  };
  fp5 = {
    syncthing.id = "7F5DOOF-FWVPO3U-IX5LGHL-OQBL5DA-SG47QUF-N7ECRH4-EEHX76Z-KU7Q3QL";
  };
}
