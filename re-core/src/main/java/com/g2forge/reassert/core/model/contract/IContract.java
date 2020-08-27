package com.g2forge.reassert.core.model.contract;

import com.g2forge.alexandria.java.adt.name.IStringNamed;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.contract.terms.ITerms;

public interface IContract extends IStringNamed, IVertex {
	public ITerms<?> getTerms();

	@Override
	public default boolean isMaterial() {
		return false;
	}
}
