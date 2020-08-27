package com.g2forge.reassert.contract.model.logic;

import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.reassert.core.model.contract.ITerm;
import com.g2forge.reassert.core.model.contract.TermRelation;

import lombok.Builder;
import lombok.Data;
import lombok.RequiredArgsConstructor;

@Data
@Builder(toBuilder = true)
@RequiredArgsConstructor
public class ContextTerm implements ITerm {
	protected final String name;

	protected final IFunction1<?, TermRelation> function;

	@Override
	public String getDescription() {
		throw new UnsupportedOperationException();
	}

	@Override
	public String getName() {
		return name;
	}
}