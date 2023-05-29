## Initial state.
In the initial state, both Gunicorn and Vue expose HTTPS on both the loopback (127.0.0.1) 
and network (172.x.x.x) interfaces and manage their own HTTPS configuration.

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

This creates a hassle during local development as HTTPS either needs to be disabled or configured.
Disabling HTTPS, while easy, creates unnecessary differences in GitHub.
Configuring HTTPS for local development is unnecessary work.

## Terminate https
I thought it might be beneficial to terminate HTTPS on Nginx and have Gunicorn and Vue always use plain HTTP.

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

## Complication #1: Vue doesn't give up the network port.

Unfortunately, Vue doesn't give up the network port. So, after encountering some troubles,
I had to set up a less straightforward scheme where Nginx proxies the ports by adding 1 to the port number. 
So HTTPS:8400 is forwarded to HTTP:18400.

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

## Complication #2: Vue needs to proxy WebSockets too.
Vue relies on WebSockets for communication between the client (web browser) and the server in development mode. 
Therefore, WebSockets need to be proxied as well.

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

This creates a further complication that when Vue is started in HTTP mode, 
it creates a dynamically unsecured WebSocket backlink. By default, the Vue client in the 
browser will try to communicate with the Vue server on `ws://host:18401`, which is not served. 
This can be explicitly configured in `vue.config.js`:

```javascript
devServer: {
    client: {
        webSocketURL: 'wss://cclaw.legalese.com:8408/ws'
    }
}
```
However, it needs to know the port on the proxy. This can still be done by templating `vue.config`
during the generation of the `vue-0xx` work directory, as the external port is known at the time of 
`vue-0xx` generation. But it further complicates the setup and adds some more "magic".

Basically, what was expected to be a simple proxy setup that simplifies the configuration turned 
out to be a rather complex setup relying on non-standard configurations and runtime template generation.