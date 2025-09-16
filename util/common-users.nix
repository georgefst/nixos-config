{
  users = {
    mutableUsers = false;
    groups = {
      gthomas = { };
    };
    users = {
      root = { };
      gthomas = {
        isNormalUser = true;
        linger = true;
        createHome = true;
        home = "/home/gthomas";
        group = "gthomas";
        extraGroups = [
          "wheel"
          "input"
        ];
        hashedPassword = "$6$jgaC/YaKr634BoKQ$KIv3VvRRaYShRibX5O3lAaqZ2qE3XRcYQEd0EF6YP61a9YBYUcPtljpDPE8.wEnMDNeUw9/ePBjsrK9JUv5i5/";
      };
    };
  };
}
