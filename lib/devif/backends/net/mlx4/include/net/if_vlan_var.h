/*
 * Copyright 1998 Massachusetts Institute of Technology
 *
 * Permission to use, copy, modify, and distribute this software and
 * its documentation for any purpose and without fee is hereby
 * granted, provided that both the above copyright notice and this
 * permission notice appear in all copies, that both the above
 * copyright notice and this permission notice appear in all
 * supporting documentation, and that the name of M.I.T. not be used
 * in advertising or publicity pertaining to distribution of the
 * software without specific, written prior permission.  M.I.T. makes
 * no representations about the suitability of this software for any
 * purpose.  It is provided "as is" without express or implied
 * warranty.
 *
 * THIS SOFTWARE IS PROVIDED BY M.I.T. ``AS IS''.  M.I.T. DISCLAIMS
 * ALL EXPRESS OR IMPLIED WARRANTIES WITH REGARD TO THIS SOFTWARE,
 * INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF
 * MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE. IN NO EVENT
 * SHALL M.I.T. BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
 * SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
 * LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF
 * USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
 * ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 *
 * $FreeBSD: src/sys/net/if_vlan_var.h,v 1.11 2002/03/11 09:26:07 mux Exp $
 */

#ifndef _NET_IF_VLAN_VAR_H_
#define	_NET_IF_VLAN_VAR_H_	1

#ifdef _KERNEL
struct vlan_mc_entry {
	struct ether_addr mc_addr;
	SLIST_ENTRY(vlan_mc_entry) mc_entries;
};

struct ifvlan {
	struct arpcom ifv_ac; /* make this an interface */
	struct ifnet *ifv_p; /* parent inteface of this vlan */
	struct ifv_linkmib {
		int ifvm_parent;
		u_int16_t ifvm_proto; /* encapsulation ethertype */
		u_int16_t ifvm_tag; /* tag to apply on packets leaving if */
	}ifv_mib;
	SLIST_HEAD(__vlan_mchead, vlan_mc_entry) vlan_mc_listhead;
	LIST_ENTRY(ifvlan) ifv_list;
};
#define	ifv_if	ifv_ac.ac_if
#define	ifv_tag	ifv_mib.ifvm_tag
#endif /* _KERNEL */

struct ether_vlan_header {
	u_char evl_dhost[ETHER_ADDR_LEN];
	u_char evl_shost[ETHER_ADDR_LEN];
	u_int16_t evl_encap_proto;
	u_int16_t evl_tag;
	u_int16_t evl_proto;
};

#define	EVL_VLANOFTAG(tag) ((tag) & 4095)
#define	EVL_PRIOFTAG(tag) (((tag) >> 13) & 7)
#define	EVL_ENCAPLEN	4	/* length in octets of encapsulation */

/* sysctl(3) tags, for compatibility purposes */
#define	VLANCTL_PROTO	1
#define	VLANCTL_MAX	2

/*
 * Configuration structure for SIOCSETVLAN and SIOCGETVLAN ioctls.
 */
/*struct	vlanreq {
 char	vlr_parent[IFNAMSIZ];
 u_short	vlr_tag;
 };*/
#define	SIOCSETVLAN	SIOCSIFGENERIC
#define	SIOCGETVLAN	SIOCGIFGENERIC

#endif /* _NET_IF_VLAN_VAR_H_ */
