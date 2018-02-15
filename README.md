`stack build && stack exec rb-complexity-hs-exe | python -m json.tool`

~~~~
[
    {
        "ccost": 354,
        "cmap": {
            "Bunny::Channel#basic_consume": 27.5,
            "Bunny::Channel#basic_consume_with": 32.8,
            "Bunny::Channel#basic_get": 15.7,
            "Bunny::Channel#basic_publish": 21.1,
            "Bunny::Channel#basic_qos": 16.6,
            "Bunny::Channel#confirm_select": 16.9,
            "Bunny::Channel#exchange_bind": 16.8,
            "Bunny::Channel#exchange_declare": 17.4,
            "Bunny::Channel#exchange_unbind": 16.8,
            "Bunny::Channel#handle_ack_or_nack": 18.3,
            "Bunny::Channel#handle_frameset": 13.5,
            "Bunny::Channel#handle_method": 87.6,
            "Bunny::Channel#initialize": 42.2,
            "Bunny::Channel#instantiate_channel_level_exception": 10.8
        },
        "cspec": "Bunny::Channel#basic_consume"
    },
    {
        "ccost": 31.3,
        "cmap": {
            "Bunny::Channel#none": 31.3
        },
        "cspec": "Bunny::Channel#none"
    },
    {
        "ccost": 142.7,
        "cmap": {
            "Bunny::Channel#queue_bind": 16,
            "Bunny::Channel#queue_declare": 13.4,
            "Bunny::Channel#queue_delete": 11.5,
            "Bunny::Channel#queue_unbind": 14.1,
            "Bunny::Channel#recover_consumers": 12.3,
            "Bunny::Channel#to_s": 15.7,
            "Bunny::Channel#wait_on_basic_get_continuations": 17.1,
            "Bunny::Channel#wait_on_confirms_continuations": 25.5,
            "Bunny::Channel#wait_on_continuations": 17.1
        },
        "cspec": "Bunny::Channel#queue_bind"
    },
    {
        "ccost": 11,
        "cmap": {
            "Bunny::Concurrent::AtomicFixnum#none": 11
        },
        "cspec": "Bunny::Concurrent::AtomicFixnum#none"
    },
    {
        "ccost": 12.4,
        "cmap": {
            "Bunny::Concurrent::ContinuationQueue#poll": 12.4
        },
        "cspec": "Bunny::Concurrent::ContinuationQueue#poll"
    },
    {
        "ccost": 11.6,
        "cmap": {
            "Bunny::Consumer#initialize": 11.6
        },
        "cspec": "Bunny::Consumer#initialize"
    },
    {
        "ccost": 54.2,
        "cmap": {
            "Bunny::ConsumerWorkPool#initialize": 11.7,
            "Bunny::ConsumerWorkPool#run_loop": 20.2,
            "Bunny::ConsumerWorkPool#shutdown": 11.3,
            "Bunny::ConsumerWorkPool#start": 11
        },
        "cspec": "Bunny::ConsumerWorkPool#initialize"
    },
    {
        "ccost": 18.1,
        "cmap": {
            "Bunny::Exchange#initialize": 18.1
        },
        "cspec": "Bunny::Exchange#initialize"
    },
    {
        "ccost": 40,
        "cmap": {
            "Bunny::Framing::IO::Frame::decode": 19.9,
            "Bunny::Framing::String::Frame::decode": 20.1
        },
        "cspec": "Bunny::Framing::IO::Frame::decode"
    },
    {
        "ccost": 25.3,
        "cmap": {
            "Bunny::HeartbeatSender#run": 12.1,
            "Bunny::HeartbeatSender#start": 13.2
        },
        "cspec": "Bunny::HeartbeatSender#run"
    },
    {
        "ccost": 24.6,
        "cmap": {
            "Bunny::JRuby::SSLSocket#read_fully": 24.6
        },
        "cspec": "Bunny::JRuby::SSLSocket#read_fully"
    },
    {
        "ccost": 15.2,
        "cmap": {
            "Bunny::JRuby::Socket#read_fully": 15.2
        },
        "cspec": "Bunny::JRuby::Socket#read_fully"
    },
    {
        "ccost": 109.3,
        "cmap": {
            "Bunny::Queue#bind": 10.3,
            "Bunny::Queue#initialize": 18.1,
            "Bunny::Queue#pop": 19.6,
            "Bunny::Queue#recover_from_network_failure": 12.5,
            "Bunny::Queue#subscribe": 29.3,
            "Bunny::Queue#unbind": 19.5
        },
        "cspec": "Bunny::Queue#bind"
    },
    {
        "ccost": 94.9,
        "cmap": {
            "Bunny::ReaderLoop#log_exception": 17.7,
            "Bunny::ReaderLoop#run_loop": 43,
            "Bunny::ReaderLoop#run_once": 34.2
        },
        "cspec": "Bunny::ReaderLoop#log_exception"
    },
    {
        "ccost": 52,
        "cmap": {
            "Bunny::SSLSocket#read_fully": 26.3,
            "Bunny::SSLSocket#write_nonblock_fully": 25.7
        },
        "cspec": "Bunny::SSLSocket#read_fully"
    },
    {
        "ccost": 296.99997,
        "cmap": {
            "Bunny::Session#addresses_from": 16.8,
            "Bunny::Session#close_all_channels": 14.4,
            "Bunny::Session#close_channel": 12.6,
            "Bunny::Session#close_connection": 10.4,
            "Bunny::Session#close_transport": 11.9,
            "Bunny::Session#create_channel": 13.9,
            "Bunny::Session#handle_frame": 46.9,
            "Bunny::Session#handle_frameset": 13,
            "Bunny::Session#handle_network_failure": 20.8,
            "Bunny::Session#host_from_address": 15.2,
            "Bunny::Session#init_connection": 13.7,
            "Bunny::Session#initialize": 73.2,
            "Bunny::Session#initialize_transport": 15.3,
            "Bunny::Session#instantiate_connection_level_exception": 18.9
        },
        "cspec": "Bunny::Session#addresses_from"
    },
    {
        "ccost": 22.9,
        "cmap": {
            "Bunny::Session#none": 22.9
        },
        "cspec": "Bunny::Session#none"
    },
    {
        "ccost": 206.9,
        "cmap": {
            "Bunny::Session#open?": 12.8,
            "Bunny::Session#open_channel": 11.2,
            "Bunny::Session#open_connection": 95.6,
            "Bunny::Session#port_from_address": 18.8,
            "Bunny::Session#recover_from_network_failure": 21.8,
            "Bunny::Session#start": 34.6,
            "Bunny::Session#validate_connection_options": 12.1
        },
        "cspec": "Bunny::Session#open?"
    },
    {
        "ccost": 48.9,
        "cmap": {
            "Bunny::Socket#read_fully": 15.2,
            "Bunny::Socket#write_nonblock_fully": 14.7,
            "Bunny::Socket::open": 19
        },
        "cspec": "Bunny::Socket#read_fully"
    },
    {
        "ccost": 203.00002,
        "cmap": {
            "Bunny::Transport#initialize": 24.9,
            "Bunny::Transport#initialize_tls_certificate_store": 27.3,
            "Bunny::Transport#initialize_tls_context": 20.1,
            "Bunny::Transport#prepare_tls_context": 19.7,
            "Bunny::Transport#read_fully": 15.4,
            "Bunny::Transport#read_next_frame": 18.8,
            "Bunny::Transport#tls_certificate_path_from": 10.8,
            "Bunny::Transport#tls_enabled?": 10.3,
            "Bunny::Transport#write": 42.9,
            "Bunny::Transport#write_without_timeout": 12.8
        },
        "cspec": "Bunny::Transport#initialize"
    },
    {
        "ccost": 3121.1,
        "cmap": {
            "flog": 3121.1
        },
        "cspec": "flog"
    },
    {
        "ccost": 6.8,
        "cmap": {
            "flog/method": 6.8
        },
        "cspec": "flog/method"
    },
    {
        "ccost": 105.9,
        "cmap": {
            "main#none": 105.9
        },
        "cspec": "main#none"
    }
][
    {
        "ccost": 354,
        "cmap": {
            "Bunny::Channel#basic_consume": 27.5,
            "Bunny::Channel#basic_consume_with": 32.8,
            "Bunny::Channel#basic_get": 15.7,
            "Bunny::Channel#basic_publish": 21.1,
            "Bunny::Channel#basic_qos": 16.6,
            "Bunny::Channel#confirm_select": 16.9,
            "Bunny::Channel#exchange_bind": 16.8,
            "Bunny::Channel#exchange_declare": 17.4,
            "Bunny::Channel#exchange_unbind": 16.8,
            "Bunny::Channel#handle_ack_or_nack": 18.3,
            "Bunny::Channel#handle_frameset": 13.5,
            "Bunny::Channel#handle_method": 87.6,
            "Bunny::Channel#initialize": 42.2,
            "Bunny::Channel#instantiate_channel_level_exception": 10.8
        },
        "cspec": "Bunny::Channel"
    },
    {
        "ccost": 31.3,
        "cmap": {
            "Bunny::Channel#none": 31.3
        },
        "cspec": "Bunny::Channel"
    },
    {
        "ccost": 142.7,
        "cmap": {
            "Bunny::Channel#queue_bind": 16,
            "Bunny::Channel#queue_declare": 13.4,
            "Bunny::Channel#queue_delete": 11.5,
            "Bunny::Channel#queue_unbind": 14.1,
            "Bunny::Channel#recover_consumers": 12.3,
            "Bunny::Channel#to_s": 15.7,
            "Bunny::Channel#wait_on_basic_get_continuations": 17.1,
            "Bunny::Channel#wait_on_confirms_continuations": 25.5,
            "Bunny::Channel#wait_on_continuations": 17.1
        },
        "cspec": "Bunny::Channel"
    },
    {
        "ccost": 11,
        "cmap": {
            "Bunny::Concurrent::AtomicFixnum#none": 11
        },
        "cspec": "Bunny::Concurrent::AtomicFixnum"
    },
    {
        "ccost": 12.4,
        "cmap": {
            "Bunny::Concurrent::ContinuationQueue#poll": 12.4
        },
        "cspec": "Bunny::Concurrent::ContinuationQueue"
    },
    {
        "ccost": 11.6,
        "cmap": {
            "Bunny::Consumer#initialize": 11.6
        },
        "cspec": "Bunny::Consumer"
    },
    {
        "ccost": 54.2,
        "cmap": {
            "Bunny::ConsumerWorkPool#initialize": 11.7,
            "Bunny::ConsumerWorkPool#run_loop": 20.2,
            "Bunny::ConsumerWorkPool#shutdown": 11.3,
            "Bunny::ConsumerWorkPool#start": 11
        },
        "cspec": "Bunny::ConsumerWorkPool"
    },
    {
        "ccost": 18.1,
        "cmap": {
            "Bunny::Exchange#initialize": 18.1
        },
        "cspec": "Bunny::Exchange"
    },
    {
        "ccost": 40,
        "cmap": {
            "Bunny::Framing::IO::Frame::decode": 19.9,
            "Bunny::Framing::String::Frame::decode": 20.1
        },
        "cspec": "Bunny::Framing::IO::Frame::decode"
    },
    {
        "ccost": 25.3,
        "cmap": {
            "Bunny::HeartbeatSender#run": 12.1,
            "Bunny::HeartbeatSender#start": 13.2
        },
        "cspec": "Bunny::HeartbeatSender"
    },
    {
        "ccost": 24.6,
        "cmap": {
            "Bunny::JRuby::SSLSocket#read_fully": 24.6
        },
        "cspec": "Bunny::JRuby::SSLSocket"
    },
    {
        "ccost": 15.2,
        "cmap": {
            "Bunny::JRuby::Socket#read_fully": 15.2
        },
        "cspec": "Bunny::JRuby::Socket"
    },
    {
        "ccost": 109.3,
        "cmap": {
            "Bunny::Queue#bind": 10.3,
            "Bunny::Queue#initialize": 18.1,
            "Bunny::Queue#pop": 19.6,
            "Bunny::Queue#recover_from_network_failure": 12.5,
            "Bunny::Queue#subscribe": 29.3,
            "Bunny::Queue#unbind": 19.5
        },
        "cspec": "Bunny::Queue"
    },
    {
        "ccost": 94.9,
        "cmap": {
            "Bunny::ReaderLoop#log_exception": 17.7,
            "Bunny::ReaderLoop#run_loop": 43,
            "Bunny::ReaderLoop#run_once": 34.2
        },
        "cspec": "Bunny::ReaderLoop"
    },
    {
        "ccost": 52,
        "cmap": {
            "Bunny::SSLSocket#read_fully": 26.3,
            "Bunny::SSLSocket#write_nonblock_fully": 25.7
        },
        "cspec": "Bunny::SSLSocket"
    },
    {
        "ccost": 296.99997,
        "cmap": {
            "Bunny::Session#addresses_from": 16.8,
            "Bunny::Session#close_all_channels": 14.4,
            "Bunny::Session#close_channel": 12.6,
            "Bunny::Session#close_connection": 10.4,
            "Bunny::Session#close_transport": 11.9,
            "Bunny::Session#create_channel": 13.9,
            "Bunny::Session#handle_frame": 46.9,
            "Bunny::Session#handle_frameset": 13,
            "Bunny::Session#handle_network_failure": 20.8,
            "Bunny::Session#host_from_address": 15.2,
            "Bunny::Session#init_connection": 13.7,
            "Bunny::Session#initialize": 73.2,
            "Bunny::Session#initialize_transport": 15.3,
            "Bunny::Session#instantiate_connection_level_exception": 18.9
        },
        "cspec": "Bunny::Session"
    },
    {
        "ccost": 22.9,
        "cmap": {
            "Bunny::Session#none": 22.9
        },
        "cspec": "Bunny::Session"
    },
    {
        "ccost": 206.9,
        "cmap": {
            "Bunny::Session#open?": 12.8,
            "Bunny::Session#open_channel": 11.2,
            "Bunny::Session#open_connection": 95.6,
            "Bunny::Session#port_from_address": 18.8,
            "Bunny::Session#recover_from_network_failure": 21.8,
            "Bunny::Session#start": 34.6,
            "Bunny::Session#validate_connection_options": 12.1
        },
        "cspec": "Bunny::Session"
    },
    {
        "ccost": 48.9,
        "cmap": {
            "Bunny::Socket#read_fully": 15.2,
            "Bunny::Socket#write_nonblock_fully": 14.7,
            "Bunny::Socket::open": 19
        },
        "cspec": "Bunny::Socket"
    },
    {
        "ccost": 203.00002,
        "cmap": {
            "Bunny::Transport#initialize": 24.9,
            "Bunny::Transport#initialize_tls_certificate_store": 27.3,
            "Bunny::Transport#initialize_tls_context": 20.1,
            "Bunny::Transport#prepare_tls_context": 19.7,
            "Bunny::Transport#read_fully": 15.4,
            "Bunny::Transport#read_next_frame": 18.8,
            "Bunny::Transport#tls_certificate_path_from": 10.8,
            "Bunny::Transport#tls_enabled?": 10.3,
            "Bunny::Transport#write": 42.9,
            "Bunny::Transport#write_without_timeout": 12.8
        },
        "cspec": "Bunny::Transport"
    },
    {
        "ccost": 3121.1,
        "cmap": {
            "flog": 3121.1
        },
        "cspec": "flog"
    },
    {
        "ccost": 6.8,
        "cmap": {
            "flog/method": 6.8
        },
        "cspec": "flog/method"
    },
    {
        "ccost": 105.9,
        "cmap": {
            "main#none": 105.9
        },
        "cspec": "main"
    }
]
~~~~
>
