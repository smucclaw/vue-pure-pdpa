## Initial state.

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