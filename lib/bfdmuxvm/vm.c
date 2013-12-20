/**
 * \file
 * \brief Implements a virtual machine for executing compiled intermediate language byte code
 *
 */
/*
 * Copyright (c) 2009, 2010, ETH Zurich.
 * All rights reserved.
 *
 * This file is distributed under the terms in the attached LICENSE file.
 * If you do not find this file, copies can be found by writing to:
 * ETH Zurich D-INFK, Haldeneggsteig 4, CH-8092 Zurich. Attn: Systems Group.
 */

#include <sys/endian.h>
#include <bfdmuxvm/vm.h>
#include <bfdmuxtools/bfdmux.h>

#include <barrelfish/barrelfish.h>


/**
 * Read data from possibly scattered packet
 *
 * @param data   Array of pointers to each buffer
 * @param len    Array of sizes for each buffer
 * @param count  Number of buffers
 * @param offset Offset to start of region to be read
 * @param size   Number of bytes to be read
 * @param dst    Memory location for result
 *
 * @return 1 if the data has been read successfully, 0 otherwise.
 *
 * @note Even if the result does not indicate success, the contents of dst may
 * be modified.
 **/
static inline int pkt_read(uint8_t **data, int *len, int count, int offset,
                           size_t size, void *dst)
{
    uint8_t *b = dst;
    int i = 0;
    int l = 0;
    while (size > 0) {
        while (i < count && offset >= (l + len[i])) {
            l += len[i];
            i++;
        }

        // Outside of packet boundaries
        if (i >= count) {
            printf("Ouside of boundary\n");
            return 0;
        }
        *b = data[i][offset - l];

        size--;
        offset++;
        b++;
    }
    return 1;
}

static inline int pkt_get_8(uint8_t **data, int *len, int count, int offset,
                            uint8_t *dst)
{
    return pkt_read(data, len, count, offset, 1, dst);
}

static inline int pkt_get_16(uint8_t **data, int *len, int count, int offset,
                             uint16_t *dst)
{
    return pkt_read(data, len, count, offset, 2, dst);
}

static inline int pkt_get_32(uint8_t **data, int *len, int count, int offset,
                             uint32_t *dst)
{
    return pkt_read(data, len, count, offset, 4, dst);
}

static inline int pkt_get_64(uint8_t **data, int *len, int count, int offset,
                             uint64_t *dst)
{
    return pkt_read(data, len, count, offset, 8, dst);
}


/*
 * Take compiled filter and run data through it.
 * Result gets stored in the filterresult_t*
 *
 */
/**
 * \brief Performs recursive execution of a subtree of the filter code
 * @param filter_code Points to the begining of the filter code
 * @param filter_len Specifies the length of the filter code in bytes
 * @param packet_data Array of pointers to the packet data to run the filter on
 * @param packet_len  Array of lengths of packet data in bytes
 * @param count       Number of packet parts
 * @param[out] result_value Return value of the subtree execution
 * @param[in] result_offset Initially specifies the offset of the next byte to be executed in the filter code
 * @param[out] result_offset Specifies the next code byte to be executed, after the entire subtree code was executed
 * @return ERR_OK on success, other error values on failure; see header file for error types.
 */

err_t
calc(uint8_t * filter_code, int filter_len, uint8_t **packet_data,
	 int *packet_len, int count, uint64_t * result_value,
     size_t * result_offset);
err_t
calc(uint8_t * filter_code, int filter_len, uint8_t **packet_data,
	 int *packet_len, int count, uint64_t * result_value,
     size_t * result_offset)
{
	uint64_t        res;
	uint8_t         res8;
	uint16_t        res16;
	uint32_t        res32;
	size_t          start = *result_offset;
	if (start > filter_len){
	    return ERR_UNKNOWN;
        }

	op_t            op = filter_code[*result_offset];
	err_t           err;
	switch (op) {
		/*
		 * ---------------------
		 */
		/*
		 * Single argument calls
		 */
		/*
		 * ---------------------
		 */


		// Immediates
	case OP_INT8:
		*result_value = *(uint8_t *) (filter_code + start + 1);
		*result_offset += 1;
		return ERR_OK;
	case OP_INT16:
		*result_value = *(uint16_t *) (filter_code + start + 1);
		*result_offset += 2;
		return ERR_OK;
	case OP_INT32:
		*result_value = *(uint32_t *) (filter_code + start + 1);
		*result_offset += 4;
		return ERR_OK;
	case OP_INT64:
		*result_value = *(uint64_t *) (filter_code + start + 1);
		*result_offset += 8;
		return ERR_OK;

		/*
		 * -----------------------------------------------------------------------------------
		 */
		/*
		 * Special two argument calls (maybe we can break after evaluating
		 * the first argument)
		 */
		/*
		 * -----------------------------------------------------------------------------------
		 */

		// Logical
		//
		// case OP_AND and OP_OR could be merged toghether
	case OP_AND:				// Has a 32bit argument describing the
		// whole treesize
		*result_offset += 5;
		if ((err =
			 calc(filter_code, filter_len, packet_data, packet_len, count,
				  result_value, result_offset)) < 0)
			return err;
		// Return false if first tree returned false
		if (!(*result_value)) {
			uint64_t       *subtreesize =
				(uint64_t *) ((filter_code) + start + 1);
			*result_offset = start + 4 + *subtreesize;
			// False is already in result->value
			return ERR_OK;
		}
		// Fetch 2nd argument
		*result_offset += 1;
		if ((err =
			 calc(filter_code, filter_len, packet_data, packet_len, count,
				  result_value, result_offset)) < 0)
			return err;

		// if (result->value) return true; // Result is already in
		// result->value
		// else return false;
		return ERR_OK;

	case OP_OR:				// Has a 32bit argument describing the
		// whole treesize
		*result_offset += 5;
		if ((err =
			 calc(filter_code, filter_len, packet_data, packet_len, count,
				  result_value, result_offset)) < 0)
			return err;
		// Return true if first subtree returned true
		if (*result_value) {
			uint64_t       *subtreesize =
				(uint64_t *) ((filter_code) + start + 1);
			*result_offset = start + 4 + *subtreesize;
			// True is already in result->value
			return ERR_OK;
		}
		// Fetch 2nd argument
		*result_offset += 1;
		if ((err =
			 calc(filter_code, filter_len, packet_data, packet_len, count,
				  result_value, result_offset)) < 0)
			return err;

		// if (result->value) return true; // Result is already in
		// result->value
		// else return false;
		return ERR_OK;




	default:
		/*
		 * -------------------
		 */
		/*
		 * One argument calls
		 */
		/*
		 * -------------------
		 */
		*result_offset += 1;
		if ((err =
			 calc(filter_code, filter_len, packet_data, packet_len, count,
				  result_value, result_offset)) < 0)
			return err;

		switch (op) {			/* We need only the first argument */
		case OP_NOT:
			if (*result_value) {
				*result_value = 0;
			} else {
				*result_value = 1;
			}
			return ERR_OK;
		case OP_LOAD8:
			// Packet access
            if (!pkt_get_8(packet_data, packet_len, count, *result_value,
                           &res8))
                return ERR_BAD_ACCESS; // Access not withing packet
			*result_value = res8;
			return ERR_OK;
		case OP_LOAD16:
			// Packet access
            if (!pkt_get_16(packet_data, packet_len, count, *result_value,
                            &res16))
                return ERR_BAD_ACCESS; // Access not withing packet
			*result_value = ntohs(res16);
			return ERR_OK;
		case OP_LOAD32:
			// Packet access
            if (!pkt_get_32(packet_data, packet_len, count, *result_value,
                            &res32))
                return ERR_BAD_ACCESS; // Access not withing packet
			*result_value = ntohl(res32);
			return ERR_OK;
		case OP_LOAD64:
			// Packet access
            if (!pkt_get_64(packet_data, packet_len, count, *result_value,
                            &res))
                return ERR_BAD_ACCESS; // Access not withing packet

			// If we are on a littleendian machine, translate from network
			//
			//
			// order (bigendian) to hostorder
			short           word = 0x0001;
			char           *byte = (char *) &word;
			if (byte[0])		// Little endian
				res = be64toh(res);
			(*result_value) = res;
			return ERR_OK;
		}

		/*
		 * -------------------
		 */
		/*
		 * two argument calls
		 */
		/*
		 * -------------------
		 */

		/*
		 * We also need the second argument
		 */
		// Fetch 2nd argument
		res = *result_value;
		*result_offset += 1;
		if ((err =
			 calc(filter_code, filter_len, packet_data, packet_len, count,
				  result_value, result_offset)) < 0)
			return err;

		switch (op) {
			// Comparision
		case OP_EQUAL:
			if (res == (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_UNEQUAL:
			if (res != *result_value)
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_SGREATER:
			if ((signed) res > (signed) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_SLESS:
			if ((signed) res < (signed) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_UGREATER:
			if ((unsigned) res > (unsigned) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_ULESS:
			if ((unsigned) res < (unsigned) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_SGREATEREQUAL:
			if ((signed) res >= (signed) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_SLESSEQUAL:
			if ((signed) res <= (signed) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_UGREATEREQUAL:
			if ((unsigned) res >= (unsigned) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;
		case OP_ULESSEQUAL:
			if ((unsigned) res <= (unsigned) (*result_value))
				*result_value = 1;
			else
				*result_value = 0;
			break;

			// Arithmetic
		case OP_ADD:
			(*result_value) += res;
			break;
		case OP_SUB:
			(*result_value) = res - (*result_value);
			break;
		case OP_MULT:
			(*result_value) *= res;
			break;
		case OP_IDIV:
			(*result_value) = res / (*result_value);
			break;
		case OP_MOD:
			(*result_value) = res % (*result_value);
			break;

			// Bitwise
		case OP_BAND:
			(*result_value) &= res;
			break;
		case OP_BOR:
			(*result_value) |= res;
			break;
		case OP_BXOR:
			(*result_value) ^= res;
			break;
			// case OP_BNOT: // This case gets handled on top (one
			// argument call)

			// Error
		default:
			return ERR_BAD_OP;
		}
		return ERR_OK;
		/*
		 * End of default case
		 */
	}
}

/**
 * \brief Executes the specified filter on the given packet.
 * @param filter_code Points to the filters byte code
 * @param filter_len Length of the byte code
 * @param packet_data Array of pointers to the packet data to run the filter on
 * @param packet_len  Array of lengths of packet data in bytes
 * @param count       Number of packet parts
 * @param[out] error_out Error information upon failure during execution
 * @return true, if the filter executed successfully and the result was not zero. false otherwise.
 */
bool
execute_filter(uint8_t * filter_code, int filter_len,
			   uint8_t **packet_data, int *packet_len, int count,
               int *error_out)
{
	err_t           err;
	uint64_t        result_value;
	size_t          result_offset;
	result_value = 0;
	result_offset = 0;
	err =
		calc(filter_code, filter_len, packet_data, packet_len, count,
			 &result_value, &result_offset);
	if (error_out) {
		*error_out = err;
	}
	if (err < 0 || !(result_value)) {
		return false;
	} else {
		return true;
	}
}
