{
  serverRegion = "ams3";
  serverSize = "s-1vcpu-1gb";
  hostName = "swarm"; # can be bogus
  authToken = builtins.readFile ./do-token.txt;
}
