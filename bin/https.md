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
    network_port_8400 --- nginx
    network_port_8401 --- nginx

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
    linkStyle 4 stroke:blue;
    linkStyle 5 stroke:blue;
```

## Complication #1: Vue doesn't give up network port.

Unfortunately vue doesn't give up network port.
So after some troubles I had to set up a less straightforward scheme where nginx proxies ports
by adding 1 to the port number so https:8400 is forwarded to http:18400

```mermaid
flowchart LR
    subgraph network
        direction LR
        https_172_8400(( )) -- HTTPS --- network_port_8400(172:8400)
        https_172_8401(( )) -- HTTPS --- network_port_8401(172:8401)
        https_172_18401(( )) -- HTTP --- network_port_18401(172:18401)
    end
    nginx[[nginx]]
    network_port_8400 --- nginx
    network_port_8401 --- nginx

    subgraph localhost
        direction LR
        http_127_8400(( )) -- http --- loop_port_18400(127:18400)
        http_127_8401(( ))-- http --- loop_port_18401(127:18401)
    end
    gunicorn[[ gunicorn ]]
    loop_port_18400---gunicorn
    vue[[ vue ]]
    loop_port_18401---vue
    nginx---http_127_8400
    nginx---http_127_8401
    network_port_18401---vue
    linkStyle 0 stroke:red;
    linkStyle 1 stroke:red;
    linkStyle 2 stroke:blue;
    linkStyle 5 stroke:blue;
    linkStyle 6 stroke:blue;
```

Respective redirect config for nginx
```
proxy_pass http://127.0.0.1:1$server_port;
```
Might confuse people as this is an unusual redirect setup.

## Complication #2: Vue needs to proxy websocket too.

Vue relies on WebSocket for communication between client(web browser) and server in dev mode.
So it needs to be proxied too.

```mermaid
flowchart LR
    subgraph network
        direction LR
        https_172_8400(( )) -- HTTPS --- network_port_8400(172:8400)
        https_172_8401(( )) -- HTTPS --- network_port_8401(172:8401)
        wss_172_8401(( )) -- WSS --- network_port_8401(172:18401)
        http_172_8401(( )) -- HTTP --- network_port_18401(172:18401)
        ws_172_18401(( )) -- WS --- network_port_18401(172:18401)
    end
    nginx[[nginx]]
    network_port_8400 --- nginx
    network_port_8401 --- nginx

    subgraph localhost
        direction LR
        http_127_8400(( )) -- http --- loop_port_18400(127:18400)
        http_127_8401(( ))-- http --- loop_port_18401(127:18401)
        ws_127_8401(( ))-- WS --- loop_port_18401(127:18401)
    end
    gunicorn[[ gunicorn ]]
    loop_port_18400---gunicorn
    vue[[ vue ]]
    loop_port_18401---vue
    nginx---http_127_8400
    nginx---http_127_8401
    nginx---ws_127_8401
    network_port_18401---vue
    linkStyle 0 stroke:red;
    linkStyle 1 stroke:red;
    linkStyle 2 stroke:red;
    linkStyle 3 stroke:blue;
    linkStyle 4 stroke:blue;
    linkStyle 7 stroke:blue;
    linkStyle 8 stroke:blue;
    linkStyle 9 stroke:blue;
```

This creates one further complication that when vue is started in http mode it creates a WebSocket backlink dinamically
and it's unsecured one. So by default vue client in browser will try to talk to vue server on ws://host:18401 which is not served.
This can be explicitely configured in `vue.config.js`

```javascript
devServer: {
    client: {
        webSocketURL: 'wss://cclaw.legalese.com:8408/ws'
    }
}
```
But it needs to know port on the proxy. This can be still done by templating `vue.config` during generation of vue-0xx workdir as
external port is known at the time of vue-0xx generation. But it further complicates setup and adds some more "magic".

Basically what was expected to be a simple proxy setup which simplifies config turned out to be a rather complex setup relying on non-standard
configs and runtime template generation.