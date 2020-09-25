package com.g2forge.reassert.contract.convert;

import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.enigma.backend.convert.ARenderer;
import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.enigma.backend.convert.IRendering;
import com.g2forge.enigma.backend.convert.textual.ATextualRenderer;
import com.g2forge.enigma.backend.text.model.modifier.TextNestedModified.TextNestedModifiedBuilder;
import com.g2forge.reassert.contract.model.name.AContractComparisonName;
import com.g2forge.reassert.contract.model.name.BContractComparisonName;
import com.g2forge.reassert.contract.model.name.ContractComparisonName;
import com.g2forge.reassert.contract.model.name.IContractComparisonName;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.module.IContext;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ContractComparisonNameRenderer extends ATextualRenderer<IContractComparisonName, IContractComparisonNameRenderContext> {
	@Getter
	protected class LicenseUsageNameRenderContext extends ARenderContext implements IContractComparisonNameRenderContext {
		protected final IContext context;

		public LicenseUsageNameRenderContext(TextNestedModifiedBuilder builder, IContext context) {
			super(builder);
			this.context = context;
		}

		@Override
		protected IContractComparisonNameRenderContext getThis() {
			return this;
		}
	}

	protected static class LicenseUsageNameRendering extends ARenderer.ARendering<IContractComparisonName, IContractComparisonNameRenderContext, IExplicitRenderable<? super IContractComparisonNameRenderContext>> {
		@Override
		protected void extend(TypeSwitch1.FunctionBuilder<IContractComparisonName, IExplicitRenderable<? super IContractComparisonNameRenderContext>> builder) {
			builder.add(ContractComparisonName.class, e -> c -> {
				c.append(e.getTerm().getDescription()).append(" in ");
				final IDescription contract = c.getContext().describe(e.getContract());
				c.append(contract.getName());
			});
			builder.add(AContractComparisonName.class, e -> c -> c.append(e.getTerm().getDescription()).append(" in ").append(e.getScheme().getAName()));
			builder.add(BContractComparisonName.class, e -> c -> c.append(e.getTerm().getDescription()).append(" in ").append(e.getScheme().getBName()));
		}
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final IRendering<IContractComparisonName, IContractComparisonNameRenderContext, IExplicitRenderable<? super IContractComparisonNameRenderContext>> renderingStatic = new LicenseUsageNameRendering();

	protected final IContext context;

	@Override
	protected IContractComparisonNameRenderContext createContext(TextNestedModifiedBuilder builder) {
		return new LicenseUsageNameRenderContext(builder, getContext());
	}

	@Override
	protected IRendering<? super IContractComparisonName, ? extends IContractComparisonNameRenderContext, ? extends IExplicitRenderable<? super IContractComparisonNameRenderContext>> getRendering() {
		return getRenderingStatic();
	}
}
