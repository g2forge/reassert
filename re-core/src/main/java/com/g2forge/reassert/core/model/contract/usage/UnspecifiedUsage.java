package com.g2forge.reassert.core.model.contract.usage;

import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.core.model.contract.terms.Terms;

/**
 * Indicates that no usage was specified.
 */
public class UnspecifiedUsage implements IUsageSpecific, ISingleton {
	protected static final UnspecifiedUsage INSTANCE = new UnspecifiedUsage();

	public static UnspecifiedUsage create() {
		return INSTANCE;
	}

	private UnspecifiedUsage() {}

	@Override
	public String getName() {
		return "Unspecified Usage";
	}

	@Override
	public String getShortID() {
		return "UnspecifiedUsage";
	}

	/**
	 * An unspecified usage has no specified terms, which means usage isn't possible.
	 */
	@Override
	public ITerms<IUsageTerm> getTerms() {
		return Terms.createNone();
	}
}
