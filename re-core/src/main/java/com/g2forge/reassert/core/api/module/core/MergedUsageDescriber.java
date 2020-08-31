package com.g2forge.reassert.core.api.module.core;

import com.g2forge.alexandria.java.core.helpers.HBinary;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.contract.IContractDescriber;
import com.g2forge.reassert.core.model.contract.usage.IUsage;
import com.g2forge.reassert.core.model.contract.usage.MergedUsage;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@RequiredArgsConstructor
@Getter(AccessLevel.PROTECTED)
public class MergedUsageDescriber implements IContractDescriber<MergedUsage>, ISingleton {
	protected final IContext context;

	@Getter
	protected final ITypeRef<MergedUsage> type = ITypeRef.of(MergedUsage.class);

	@Override
	public IDescription describe(MergedUsage value) {
		return new IDescription() {
			@Getter(lazy = true)
			private final String code = computeCode();

			protected String computeCode() {
				final IContext context = getContext();
				long sum = 0;
				for (IUsage usage : value.getUsages()) {
					sum += context.describe(usage).getIdentifier().hashCode();
				}
				final int hashCode = (int) ((sum & 0xFFFFFFFF) ^ (sum >>> 32));
				final byte[] bytes = HBinary.toBytes(hashCode);
				return HBinary.toHex(bytes);
			}

			@Override
			public String getIdentifier() {
				return "mergedusage-" + getCode();
			}

			@Override
			public String getName() {
				return "Merged Usage (" + getCode() + ")";
			}
		};
	}
}