package codanet

import (
	"bytes"
	"context"
	"fmt"
	"log"
	gonet "net"
	"path"
	"time"

	dsb "github.com/ipfs/go-ds-badger"
	logging "github.com/ipfs/go-log"
	p2p "github.com/libp2p/go-libp2p"
	p2pconnmgr "github.com/libp2p/go-libp2p-connmgr"
	"github.com/libp2p/go-libp2p-core/connmgr"
	"github.com/libp2p/go-libp2p-core/control"
	"github.com/libp2p/go-libp2p-core/crypto"
	"github.com/libp2p/go-libp2p-core/host"
	"github.com/libp2p/go-libp2p-core/metrics"
	"github.com/libp2p/go-libp2p-core/network"
	"github.com/libp2p/go-libp2p-core/peer"
	"github.com/libp2p/go-libp2p-core/routing"
	discovery "github.com/libp2p/go-libp2p-discovery"
	dht "github.com/libp2p/go-libp2p-kad-dht"
	"github.com/libp2p/go-libp2p-kad-dht/dual"
	"github.com/libp2p/go-libp2p-peerstore/pstoreds"
	pubsub "github.com/libp2p/go-libp2p-pubsub"
	record "github.com/libp2p/go-libp2p-record"
	p2pconfig "github.com/libp2p/go-libp2p/config"
	mdns "github.com/libp2p/go-libp2p/p2p/discovery"
	ma "github.com/multiformats/go-multiaddr"
	manet "github.com/multiformats/go-multiaddr/net"
	"golang.org/x/crypto/blake2b"

	libp2pmplex "github.com/libp2p/go-libp2p-mplex"
	mplex "github.com/libp2p/go-mplex"
)

func parseCIDR(cidr string) gonet.IPNet {
	_, ipnet, err := gonet.ParseCIDR(cidr)
	if err != nil {
		panic(err)
	}
	return *ipnet
}

var (
	privateCIDRs = []string{
		"10.0.0.0/8",
		"172.16.0.0/12",
		"192.168.0.0/16",
		"100.64.0.0/10",
		"198.18.0.0/15",
		"169.254.0.0/16",
	}
	privateIpFilter *ma.Filters = nil
)

func initPrivateIpFilter() {
	privateIpFilter = ma.NewFilters()
	for _, cidr := range privateCIDRs {
		privateIpFilter.AddFilter(parseCIDR(cidr), ma.ActionDeny)
	}
}

func Init() {
	initPrivateIpFilter()
}

func isPrivateAddr(addr ma.Multiaddr) bool {
	return !privateIpFilter.AddrBlocked(addr)
}

type CodaConnectionManager struct {
	p2pManager   *p2pconnmgr.BasicConnMgr
	OnConnect    func(network.Network, network.Conn)
	OnDisconnect func(network.Network, network.Conn)
}

func newCodaConnectionManager(maxConnections int) *CodaConnectionManager {
	noop := func(net network.Network, c network.Conn) {}

	return &CodaConnectionManager{
		p2pManager:   p2pconnmgr.NewConnManager(25, maxConnections, time.Duration(30*time.Second)),
		OnConnect:    noop,
		OnDisconnect: noop,
	}
}

// proxy connmgr.ConnManager interface to p2pconnmgr.BasicConnMgr
func (cm *CodaConnectionManager) TagPeer(p peer.ID, tag string, weight int) {
	cm.p2pManager.TagPeer(p, tag, weight)
}
func (cm *CodaConnectionManager) UntagPeer(p peer.ID, tag string) { cm.p2pManager.UntagPeer(p, tag) }
func (cm *CodaConnectionManager) UpsertTag(p peer.ID, tag string, upsert func(int) int) {
	cm.p2pManager.UpsertTag(p, tag, upsert)
}
func (cm *CodaConnectionManager) GetTagInfo(p peer.ID) *connmgr.TagInfo {
	return cm.p2pManager.GetTagInfo(p)
}
func (cm *CodaConnectionManager) TrimOpenConns(ctx context.Context) { cm.p2pManager.TrimOpenConns(ctx) }
func (cm *CodaConnectionManager) Protect(p peer.ID, tag string)     { cm.p2pManager.Protect(p, tag) }
func (cm *CodaConnectionManager) Unprotect(p peer.ID, tag string) bool {
	return cm.p2pManager.Unprotect(p, tag)
}
func (cm *CodaConnectionManager) IsProtected(p peer.ID, tag string) bool {
	return cm.p2pManager.IsProtected(p, tag)
}
func (cm *CodaConnectionManager) Close() error { return cm.p2pManager.Close() }

// proxy connmgr.Decayer interface to p2pconnmgr.BasicConnMgr (which implements connmgr.Decayer via struct inheritance)
func (cm *CodaConnectionManager) RegisterDecayingTag(name string, interval time.Duration, decayFn connmgr.DecayFn, bumpFn connmgr.BumpFn) (connmgr.DecayingTag, error) {
	// casting to Decayer here should always succeed
	decayer, _ := interface{}(cm.p2pManager).(connmgr.Decayer)
	tag, err := decayer.RegisterDecayingTag(name, interval, decayFn, bumpFn)
	return tag, err
}

// redirect Notifee() to self for notification interception
func (cm *CodaConnectionManager) Notifee() network.Notifiee { return cm }

// proxy Notifee notifications to p2pconnmgr.BasicConnMgr, intercepting Connected and Disconnected
func (cm *CodaConnectionManager) Listen(net network.Network, addr ma.Multiaddr) {
	cm.p2pManager.Notifee().Listen(net, addr)
}
func (cm *CodaConnectionManager) ListenClose(net network.Network, addr ma.Multiaddr) {
	cm.p2pManager.Notifee().ListenClose(net, addr)
}
func (cm *CodaConnectionManager) OpenedStream(net network.Network, stream network.Stream) {
	cm.p2pManager.Notifee().OpenedStream(net, stream)
}
func (cm *CodaConnectionManager) ClosedStream(net network.Network, stream network.Stream) {
	cm.p2pManager.Notifee().ClosedStream(net, stream)
}
func (cm *CodaConnectionManager) Connected(net network.Network, c network.Conn) {
	cm.OnConnect(net, c)
	cm.p2pManager.Notifee().Connected(net, c)
}
func (cm *CodaConnectionManager) Disconnected(net network.Network, c network.Conn) {
	cm.OnDisconnect(net, c)
	cm.p2pManager.Notifee().Disconnected(net, c)
}

// proxy remaining p2pconnmgr.BasicConnMgr methods for access
func (cm *CodaConnectionManager) GetInfo() p2pconnmgr.CMInfo {
	return cm.p2pManager.GetInfo()
}

// this type implements the ConnectionGating interface
// https://godoc.org/github.com/libp2p/go-libp2p-core/connmgr#ConnectionGating
// the comments of the functions below are taken from those docs.
type CodaGatingState struct {
	logger                  logging.EventLogger
	KnownPrivateAddrFilters *ma.Filters
	BannedAddrFilters       *ma.Filters
	TrustedAddrFilters      *ma.Filters
	BannedPeers             *peer.Set
	TrustedPeers            *peer.Set
}

// NewCodaGatingState returns a new CodaGatingState
func NewCodaGatingState(bannedAddrFilters *ma.Filters, trustedAddrFilters *ma.Filters, bannedPeers *peer.Set, trustedPeers *peer.Set) *CodaGatingState {
	logger := logging.Logger("codanet.CodaGatingState")

	if bannedAddrFilters == nil {
		bannedAddrFilters = ma.NewFilters()
	}

	if trustedAddrFilters == nil {
		trustedAddrFilters = ma.NewFilters()
	}

	if bannedPeers == nil {
		bannedPeers = peer.NewSet()
	}

	if trustedPeers == nil {
		trustedPeers = peer.NewSet()
	}

	// we initialize the known private addr filters to reject all ip addresses initially
	knownPrivateAddrFilters := ma.NewFilters()
	knownPrivateAddrFilters.AddFilter(parseCIDR("0.0.0.0/0"), ma.ActionDeny)

	return &CodaGatingState{
		logger:                  logger,
		BannedAddrFilters:       bannedAddrFilters,
		TrustedAddrFilters:      trustedAddrFilters,
		KnownPrivateAddrFilters: knownPrivateAddrFilters,
		BannedPeers:             bannedPeers,
		TrustedPeers:            trustedPeers,
	}
}

func (gs *CodaGatingState) MarkPrivateAddrAsKnown(addr ma.Multiaddr) {
	if isPrivateAddr(addr) && gs.KnownPrivateAddrFilters.AddrBlocked(addr) {
		gs.logger.Debugf("marking private addr %v as known", addr)

		ip, err := manet.ToIP(addr)
		if err != nil {
			panic(err)
		}

		bits := len(ip) * 8
		ipNet := gonet.IPNet{
			IP:   ip,
			Mask: gonet.CIDRMask(bits, bits),
		}
		gs.KnownPrivateAddrFilters.AddFilter(ipNet, ma.ActionAccept)
	}
}

func (gs *CodaGatingState) isPeerTrusted(p peer.ID) bool {
	return gs.TrustedPeers.Contains(p)
}

func (gs *CodaGatingState) isPeerBanned(p peer.ID) bool {
	return gs.BannedPeers.Contains(p)
}

// checks if a peer id is allowed to dial/accept
func (gs *CodaGatingState) isAllowedPeer(p peer.ID) bool {
	return gs.isPeerTrusted(p) || !gs.isPeerBanned(p)
}

func (gs *CodaGatingState) isAddrTrusted(addr ma.Multiaddr) bool {
	return !gs.TrustedAddrFilters.AddrBlocked(addr)
}

func (gs *CodaGatingState) isAddrBanned(addr ma.Multiaddr) bool {
	return gs.BannedAddrFilters.AddrBlocked(addr)
}

// checks if an address is allowed to dial/accept
func (gs *CodaGatingState) isAllowedAddr(addr ma.Multiaddr) bool {
	publicOrKnownPrivate := !isPrivateAddr(addr) || !gs.KnownPrivateAddrFilters.AddrBlocked(addr)
	return gs.isAddrTrusted(addr) || (!gs.isAddrBanned(addr) && publicOrKnownPrivate)
}

// checks if a peer is allowed to dial/accept; if the peer is in the trustlist, the address checks are overriden
func (gs *CodaGatingState) isAllowedPeerWithAddr(p peer.ID, addr ma.Multiaddr) bool {
	return gs.isPeerTrusted(p) || (gs.isAllowedPeer(p) && gs.isAllowedAddr(addr))
}

func (gs *CodaGatingState) logGate() {
	gs.logger.Debugf("gated a connection with config: %+v", gs)
}

// InterceptPeerDial tests whether we're permitted to Dial the specified peer.
//
// This is called by the network.Network implementation when dialling a peer.
func (gs *CodaGatingState) InterceptPeerDial(p peer.ID) (allow bool) {
	allow = gs.isAllowedPeer(p)

	if !allow {
		gs.logger.Infof("disallowing peer dial to: %v (peer)", p)
		gs.logGate()
	}

	return
}

// InterceptAddrDial tests whether we're permitted to dial the specified
// multiaddr for the given peer.
//
// This is called by the network.Network implementation after it has
// resolved the peer's addrs, and prior to dialling each.
func (gs *CodaGatingState) InterceptAddrDial(id peer.ID, addr ma.Multiaddr) (allow bool) {
	allow = gs.isAllowedPeerWithAddr(id, addr)

	if !allow {
		gs.logger.Infof("disallowing peer dial to: %v (peer + address)", id)
		gs.logGate()
	}

	return
}

// InterceptAccept tests whether an incipient inbound connection is allowed.
//
// This is called by the upgrader, or by the transport directly (e.g. QUIC,
// Bluetooth), straight after it has accepted a connection from its socket.
func (gs *CodaGatingState) InterceptAccept(addrs network.ConnMultiaddrs) (allow bool) {
	remoteAddr := addrs.RemoteMultiaddr()
	allow = gs.isAddrTrusted(remoteAddr) || !gs.isAddrBanned(remoteAddr)

	if !allow {
		gs.logger.Infof("refusing to accept inbound connection from addr: %v", remoteAddr)
		gs.logGate()
	}

	// If we are receiving a connection, and the remote address is private,
	// then we infer that we should be able to connect to that private address.
	if allow {
		gs.MarkPrivateAddrAsKnown(remoteAddr)
	}

	return
}

// InterceptSecured tests whether a given connection, now authenticated,
// is allowed.
//
// This is called by the upgrader, after it has performed the security
// handshake, and before it negotiates the muxer, or by the directly by the
// transport, at the exact same checkpoint.
func (gs *CodaGatingState) InterceptSecured(_ network.Direction, id peer.ID, addrs network.ConnMultiaddrs) (allow bool) {
	// note: we don't care about the direction (inbound/outbound). all
	// connections in coda are symmetric: if i am allowed to connect to
	// you, you are allowed to connect to me.
	remoteAddr := addrs.RemoteMultiaddr()
	allow = gs.isAllowedPeerWithAddr(id, remoteAddr)

	if !allow {
		gs.logger.Infof("refusing to accept inbound connection from authenticated addr: %v", remoteAddr)
		gs.logGate()
	}

	return
}

// InterceptUpgraded tests whether a fully capable connection is allowed.
//
// At this point, the connection a multiplexer has been selected.
// When rejecting a connection, the gater can return a DisconnectReason.
// Refer to the godoc on the ConnectionGating type for more information.
//
// NOTE: the go-libp2p implementation currently IGNORES the disconnect reason.
func (gs *CodaGatingState) InterceptUpgraded(network.Conn) (allow bool, reason control.DisconnectReason) {
	allow = true
	reason = control.DisconnectReason(0)
	return
}

type customValidator struct {
	Base record.Validator
}

func (cv customValidator) Validate(key string, value []byte) error {
	log.Printf("DHT Validating: %s = %s", key, value)
	return cv.Base.Validate(key, value)
}

func (cv customValidator) Select(key string, values [][]byte) (int, error) {
	log.Printf("DHT Selecting Among: %s = %s", key, bytes.Join(values, []byte("; ")))
	return cv.Base.Select(key, values)
}

// TODO: just put this into main.go?
// Helper contains all the daemon state
type Helper struct {
	Host              host.Host
	Mdns              *mdns.Service
	Dht               *dual.DHT
	Ctx               context.Context
	Pubsub            *pubsub.PubSub
	Logger            logging.EventLogger
	Rendezvous        string
	Discovery         *discovery.RoutingDiscovery
	Me                peer.ID
	GatingState       *CodaGatingState
	ConnectionManager *CodaConnectionManager
	BandwidthCounter  *metrics.BandwidthCounter
}

// MakeHelper does all the initialization to run one host
func MakeHelper(ctx context.Context, listenOn []ma.Multiaddr, externalAddr ma.Multiaddr, statedir string, pk crypto.PrivKey, networkID string, seeds []peer.AddrInfo, gatingState *CodaGatingState, maxConnections int) (*Helper, error) {
	logger := logging.Logger("codanet.Helper")

	me, err := peer.IDFromPrivateKey(pk)
	if err != nil {
		return nil, err
	}

	dso := dsb.DefaultOptions

	ds, err := dsb.NewDatastore(path.Join(statedir, "libp2p-peerstore-v0"), &dso)
	if err != nil {
		return nil, err
	}

	dsoDht := dsb.DefaultOptions
	dsDht, err := dsb.NewDatastore(path.Join(statedir, "libp2p-dht-v0"), &dsoDht)
	if err != nil {
		return nil, err
	}

	ps, err := pstoreds.NewPeerstore(ctx, ds, pstoreds.DefaultOpts())
	if err != nil {
		return nil, err
	}

	rendezvousString := fmt.Sprintf("/coda/0.0.1/%s", networkID)

	pnetKey := blake2b.Sum256([]byte(rendezvousString))

	// custom validator to omit the ipns validation.
	rv := customValidator{Base: record.NamespacedValidator{"pk": record.PublicKeyValidator{}}}

	var kad *dual.DHT

	mplex.MaxMessageSize = 1 << 30

	connManager := newCodaConnectionManager(maxConnections)
	bandwidthCounter := metrics.NewBandwidthCounter()

	host, err := p2p.New(ctx,
		p2p.Muxer("/coda/mplex/1.0.0", libp2pmplex.DefaultTransport),
		p2p.Identity(pk),
		p2p.Peerstore(ps),
		p2p.DisableRelay(),
		p2p.ConnectionGater(gatingState),
		p2p.ConnectionManager(connManager),
		p2p.ListenAddrs(listenOn...),
		p2p.AddrsFactory(func(as []ma.Multiaddr) []ma.Multiaddr {
			if externalAddr != nil {
				as = append(as, externalAddr)
			}

			fs := ma.NewFilters()
			for _, addr := range privateCIDRs {
				fs.AddFilter(parseCIDR(addr), ma.ActionDeny)
			}

			bs := []ma.Multiaddr{}
			for _, a := range as {
				if fs.AddrBlocked(a) {
					continue
				}
				bs = append(bs, a)
			}

			return bs
		}),
		p2p.NATPortMap(),
		p2p.Routing(
			p2pconfig.RoutingC(func(host host.Host) (routing.PeerRouting, error) {
				kad, err = dual.New(ctx, host,
					dual.WanDHTOption(dht.Datastore(dsDht)),
					dual.DHTOption(dht.Validator(rv)),
					dual.DHTOption(dht.BootstrapPeers(seeds...)),
					dual.DHTOption(dht.ProtocolPrefix("/coda")),
				)
				return kad, err
			})),
		p2p.UserAgent("github.com/codaprotocol/coda/tree/master/src/app/libp2p_helper"),
		p2p.PrivateNetwork(pnetKey[:]),
		p2p.BandwidthReporter(bandwidthCounter),
	)

	if err != nil {
		return nil, err
	}

	// nil fields are initialized by beginAdvertising
	return &Helper{
		Host:              host,
		Ctx:               ctx,
		Mdns:              nil,
		Dht:               kad,
		Pubsub:            nil,
		Logger:            logger,
		Rendezvous:        rendezvousString,
		Discovery:         nil,
		Me:                me,
		GatingState:       gatingState,
		ConnectionManager: connManager,
		BandwidthCounter:  bandwidthCounter,
	}, nil
}
