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
        hashedPassword = "$y$j9T$vJ0UjKdFINGa1tabMcLll/$g1WJkXkmdAufJze7Em4zMv.ee6zUW77mF1q2xcR13H9";
      };
    };
  };
}
