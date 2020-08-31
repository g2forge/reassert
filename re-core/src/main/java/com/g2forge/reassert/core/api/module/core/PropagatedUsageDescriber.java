package com.g2forge.reassert.core.api.module.core;

import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.contract.IContractDescriber;
import com.g2forge.reassert.core.model.contract.usage.PropagatedUsage;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter(AccessLevel.PROTECTED)
public class PropagatedUsageDescriber implements IContractDescriber<PropagatedUsage> {
	protected final IContext context;

	@Getter
	protected final ITypeRef<PropagatedUsage> type = ITypeRef.of(PropagatedUsage.class);

	@Override
	public IDescription describe(PropagatedUsage value) {
		final IContext context = getContext();
		final IDescription edge = context.describe(value.getEdge());
		final IDescription usage = context.describe(value.getUsage());
		return new IDescription() {
			@Override
			public String getIdentifier() {
				return "PropagatedUsage-" + edge.getIdentifier() + '-' + usage.getIdentifier();
			}

			@Override
			public String getName() {
				return "Propagated Usage (" + edge.getName() + " " + usage.getName() + ")";
			}
		};
	}
}