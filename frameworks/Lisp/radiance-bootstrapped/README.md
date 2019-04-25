# Radiance

Radiance is a web application environment, which is sort of like a web
framework, but more general, more flexible. It should let you write personal
websites and generally deployable applications easily and in such a way that
they can be used on practically any setup without having to undergo special
adaptations.

https://shirakumo.github.io/radiance/

https://github.com/Shirakumo/radiance
https://github.com/Shirakumo/radiance-bootstrap
https://github.com/Shirakumo/radiance-contribs
https://github.com/Shirakumo/radiance-tutorial

```shell
# Not necessary after the TFB suite run once
docker network create tfb

# In a terminal session
docker run -i -t --name tfb-database --network tfb --rm techempower/postgres

# In another terminal session
docker build --network tfb -f radiance-bootstrapped.dockerfile .
```
