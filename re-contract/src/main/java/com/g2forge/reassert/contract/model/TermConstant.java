package com.g2forge.reassert.contract.model;

import com.g2forge.reassert.core.model.contract.IContractTerms;
import com.g2forge.reassert.core.model.contract.terms.ITerm;
import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.core.model.contract.terms.TermRelation;
import com.g2forge.reassert.expression.express.IConstant;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class TermConstant implements IConstant<TermRelation> {
	protected final ITerm term;

	protected final IContractTerms contract;

	@Override
	public TermRelation get() {
		return getValue();
	}

	@Override
	public Object getName() {
		return this;
	}

	@Override
	public TermRelation getValue() {
		@SuppressWarnings("unchecked")
		final ITerms<ITerm> terms = (ITerms<ITerm>) contract.getTerms();
		return terms.getRelation(term);
	}
}
