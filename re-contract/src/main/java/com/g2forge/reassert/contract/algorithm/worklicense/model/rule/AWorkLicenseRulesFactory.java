package com.g2forge.reassert.contract.algorithm.worklicense.model.rule;

import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.IEdge;

import lombok.AccessLevel;
import lombok.Getter;

public abstract class AWorkLicenseRulesFactory implements IWorkLicenseRulesFactory {
	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final IFunction1<IEdge, Boolean> include = computeInclude();

	@ReassertLegalOpinion
	protected IFunction1<IEdge, Boolean> computeInclude() {
		final TypeSwitch1.FunctionBuilder<IEdge, Boolean> builder = new TypeSwitch1.FunctionBuilder<>();
		expand(builder);
		return builder.build();
	}

	protected abstract void expand(final TypeSwitch1.FunctionBuilder<IEdge, Boolean> builder);

	@Override
	public boolean isIncluded(IEdge edge, boolean outgoing) {
		return getInclude().apply(edge);
	}
}
