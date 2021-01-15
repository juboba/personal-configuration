{
  dk = "docker";
  dklc = "docker ps -l";  # List last Docker container
  dklcid = "docker ps -l -q";  # List last Docker container ID
  dklcip = "docker inspect -f \"{{.NetworkSettings.IPAddress}}\" $(docker ps -l -q)";  # Get IP of last Docker container
  dkps = "docker ps";  # List running Docker containers
  dkpsa = "docker ps -a";  # List all Docker containers
  dki = "docker images";  # List Docker images
  dkrmac = "docker rm $(docker ps -a -q)";  # Delete all Docker containers

  dkrmui = "docker images -q -f dangling=true | xargs -r docker rmi";  # Delete all untagged Docker images
  dkelc = "docker exec -it $(dklcid) bash --login"; # Enter last container (works with Docker 1.3 and above)
  dkrmflast = "docker rm -f $(dklcid)";
  dkbash = "dkelc";
  dkex = "docker exec -it "; # Useful to run any commands into container without leaving host
  dkri = "docker run --rm -i ";
  dkric = "docker run --rm -i -v $PWD:/cwd -w /cwd ";
  dkrit = "docker run --rm -it ";
  dkritc = "docker run --rm -it -v $PWD:/cwd -w /cwd ";

  # Added more recent cleanup options from newer docker versions
  dkip = "docker image prune -a -f";
  dkvp = "docker volume prune -f";
  dksp = "docker system prune -a -f";
}
