package proto

const (
	TCP_OPTION_EOL       = iota // End of Option List
	TCP_OPTION_NOP              // No-Operation
	TCP_OPTION_MSS              // Maximum Segment Size
	TCP_OPTION_WSCALE           // Window Scale
	TCP_OPTION_SACK_PERM        // SACK Permitted
	TCP_OPTION_SACK             // SACK
	TCP_OPTION_ECHO             // Echo (obsoleted by option 8)
	TCP_OPTION_ECHO_REP         // Echo Reply (obsoleted by option 8)
	TCP_OPTION_TIMESTAMP        // Timestamps
	TCP_OPTION_POC_PERM         // Partial Order Connection Permitted (obsolete)
	TCP_OPTION_POC_SP           // Partial Order Service Profile (obsolete)
)

const (
	TCP_OPTION_PREFIX_LEN    = 2
	TCP_OPTION_MSS_LEN       = 4
	TCP_OPTION_WSCALE_LEN    = 3
	TCP_OPTION_SACK_PERM_LEN = 2

	TCP_OPTION_ECHO_LEN      = 6
	TCP_OPTION_ECHO_REP_LEN  = 6
	TCP_OPTION_TIMESTAMP_LEN = 10
	TCP_OPTION_POC_PERM_LEN  = 2
	TCP_OPTION_POC_SP_LEN    = 3
)
