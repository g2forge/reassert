package com.g2forge.reassert.contract.model;

import com.g2forge.reassert.core.model.contract.IContract;
import com.g2forge.reassert.core.model.contract.ITerm;
import com.g2forge.reassert.core.model.contract.ITerms;
import com.g2forge.reassert.core.model.contract.TermRelation;
import com.g2forge.reassert.expression.express.IConstant;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class TermConstant implements IConstant<TermRelation> {
	protected final ITerm term;

	protected final IContract contract;

	@Override
	public TermRelation get() {
		return getValue();
	}

	@Override
	public String getName() {
		return String.format("%1$s in %2$s", getTerm().getDescription(), getContract().getName());
	}

	@Override
	public TermRelation getValue() {
		@SuppressWarnings("unchecked")
		final ITerms<ITerm> terms = (ITerms<ITerm>) contract.getTerms();
		return terms.getRelation(term);
	}
}
