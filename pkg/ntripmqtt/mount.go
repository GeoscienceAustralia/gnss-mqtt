package ntripmqtt

import (
	"fmt"
	"time"
)

// Mount represents a NTRIP mountpoint which proxies through to an MQTT topic
// TODO: Figure out how to populate unknown attributes in a dynamic way, this
// could involve GeodesyML / parsing the stream content
type Mount struct {
	Name          string
	Identifier    string // meta
	Format        string // stream - minor version doesn't really matter since RTCM3 is strictly additive
	FormatDetails string // stream
	Carrier       string // stream
	NavSystem     string // stream
	Network       string // meta
	CountryCode   string // meta
	Latitude      string // meta / stream
	Longitude     string // meta / stream
	//	NMEA           bool
	//	Solution       bool
	Generator string // stream
	//	Compression    string
	//	Authentication string
	//	Fee            bool
	//	Bitrate        int
	Misc        string
	LastMessage time.Time
}

// String representation of Mount in NTRIP Sourcetable entry format
func (mount *Mount) String() string {
	return fmt.Sprintf("STR;%s;%s;%s;%s;%s;%s;%s;%s;%s;%s;0;0;%s;none;N;N;0;%s",
		mount.Name, mount.Identifier, "RTCM 3", mount.FormatDetails, mount.Carrier,
		mount.NavSystem, mount.Network, mount.CountryCode, mount.Latitude, mount.Longitude,
		mount.Generator, mount.Misc)
}
