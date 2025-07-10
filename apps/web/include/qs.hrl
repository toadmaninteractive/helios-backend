-record(qs_data, {
    order_by :: binary(),
    order_dir :: binary(),
    offset :: non_neg_integer(),
    limit :: non_neg_integer(),
    filter :: undefined | jsx:json_term()
}).

-type qs_data() :: #qs_data{}.
