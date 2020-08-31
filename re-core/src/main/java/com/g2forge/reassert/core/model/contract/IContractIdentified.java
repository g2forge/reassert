package com.g2forge.reassert.core.model.contract;

import com.g2forge.alexandria.java.adt.name.IStringNamed;

public interface IContractIdentified extends IStringNamed, IContract {
	/**
	 * Get a human readable name for this contract.
	 * 
	 * @return A human readable name for this contract.
	 */
	@Override
	public String getName();

	/**
	 * Get the short identifier for this contract. This method must return the same value of as {@link #getSPDXShortID()} unless that method returns
	 * {@code null}.
	 * 
	 * @return The short identifier for this contract.
	 */
	public default String getShortID() {
		return getSPDXShortID();
	}

	/**
	 * Get the <a href="https://spdx.org/licenses/">SPDX short identifier</a> for this contract. May return {@code null} if this contract is not recognized by
	 * SPDX.
	 * 
	 * @return The SPDX short identifier for this contract.
	 */
	public String getSPDXShortID();
}
