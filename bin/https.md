## Initial state.
In initial state both gunicorn and vue expose https on both loopback(127.0.0.1)  and 
network(172.x.x.x) interfaces and manage their own https config.

```mermaid
flowchart RL
    subgraph network
        network_port_8400(172:8400)-- https ---id1(( ))
        network_port_8401(172:8401)-- https ---id2(( ))
    end
    subgraph localhost
        loop_port_8400(127:8400)-- https ---id3(( ))
        loop_port_8401(127:8401)-- https ---id4(( ))
    end
    gunicorn[[ gunicorn ]]---network_port_8400
    gunicorn---loop_port_8400
    vue[[ vue ]]---network_port_8401
    vue---loop_port_8401
```

This creates a drag during local development as https either needs to be disabled or configured.
Disabling https while easy creates a spurious diffs in github.
And configuration of https for local dev is unnecessary work.

## Terminate https
I thought it might be beneficial to terminate https on nginx and make gunicorn and vue always use plain http

```mermaid
flowchart LR
    subgraph network
        direction LR
        id1(( )) -- HTTPS --- network_port_8400(172:8400)
        id2(( )) -- HTTPS --- network_port_8401(172:8401)
    end
    nginx[[nginx]]
    network_port_8400(172:8400) --- nginx
    network_port_8401(172:8400) --- nginx

    subgraph localhost
        direction LR
        id3(( )) -- http --- loop_port_8400(127:8400)
        id4(( ))-- http --- loop_port_8401(127:8401)
    end
    gunicorn[[ gunicorn ]]
    loop_port_8400---gunicorn
    vue[[ vue ]]
    loop_port_8401---vue
    nginx---id3
    nginx---id4
    linkStyle 0 stroke:red;
    linkStyle 1 stroke:red;
    linkStyle 2 stroke:red;
    linkStyle 3 stroke:red;
    linkStyle 8 stroke:blue;
    linkStyle 9 stroke:blue;
    linkStyle 4 stroke:blue;
    linkStyle 5 stroke:blue;
```

## Complication #1 vue doesn't give up network port.

Unfortunately vue doesn't give up network port.
So after some troubles I had to set up a less straightforward scheme where nginx proxies ports
by adding 1 to the port number so https:8400 is forwarded to http:18400

```mermaid
flowchart LR
    subgraph network
        direction LR
        id1(( )) -- HTTPS --- network_port_8400(172:8400)
        id2(( )) -- HTTPS --- network_port_8401(172:8401)
        id5(( )) -- HTTP --- network_port_18401(172:18400)
    end
    nginx[[nginx]]
    network_port_8400(172:8400) --- nginx
    network_port_8401(172:8400) --- nginx

    subgraph localhost
        direction LR
        id3(( )) -- http --- loop_port_18400(127:18400)
        id4(( ))-- http --- loop_port_18401(127:18401)
    end
    gunicorn[[ gunicorn ]]
    loop_port_18400---gunicorn
    vue[[ vue ]]
    loop_port_18401---vue
    nginx---id3
    nginx---id4
    network_port_18401---vue
    linkStyle 0 stroke:red;
    linkStyle 1 stroke:red;
    linkStyle 3 stroke:red;
    linkStyle 4 stroke:red;
    linkStyle 9 stroke:blue;
    linkStyle 10 stroke:blue;
    linkStyle 5 stroke:blue;
    linkStyle 6 stroke:blue;
```

Respective redirect config for nginx
```
proxy_pass http://127.0.0.1:1$server_port;
```
Might confuse people as this is an unusual redirect setup.