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