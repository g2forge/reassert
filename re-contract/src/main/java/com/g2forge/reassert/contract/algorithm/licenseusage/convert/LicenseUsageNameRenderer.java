package com.g2forge.reassert.contract.algorithm.licenseusage.convert;

import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.enigma.backend.convert.ARenderer;
import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.enigma.backend.convert.IRendering;
import com.g2forge.enigma.backend.convert.textual.ATextualRenderer;
import com.g2forge.enigma.backend.text.model.modifier.TextNestedModified.TextNestedModifiedBuilder;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.name.ContractLicenseUsageName;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.name.ILicenseUsageName;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.name.LicenseTermLicenseUsageName;
import com.g2forge.reassert.contract.algorithm.licenseusage.model.name.UsageTermLicenseUsageName;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.module.IContext;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class LicenseUsageNameRenderer extends ATextualRenderer<ILicenseUsageName, ILicenseUsageNameRenderContext> {
	@Getter
	protected class LicenseUsageNameRenderContext extends ARenderContext implements ILicenseUsageNameRenderContext {
		protected final IContext context;

		public LicenseUsageNameRenderContext(TextNestedModifiedBuilder builder, IContext context) {
			super(builder);
			this.context = context;
		}

		@Override
		protected ILicenseUsageNameRenderContext getThis() {
			return this;
		}
	}

	protected static class LicenseUsageNameRendering extends ARenderer.ARendering<ILicenseUsageName, ILicenseUsageNameRenderContext, IExplicitRenderable<? super ILicenseUsageNameRenderContext>> {
		@Override
		protected void extend(TypeSwitch1.FunctionBuilder<ILicenseUsageName, IExplicitRenderable<? super ILicenseUsageNameRenderContext>> builder) {
			builder.add(ContractLicenseUsageName.class, e -> c -> {
				c.append(e.getTerm().getDescription()).append(" in ");
				final IDescription contract = c.getContext().describe(e.getContract());
				c.append(contract.getName());
			});
			builder.add(LicenseTermLicenseUsageName.class, e -> c -> c.append(e.getTerm().getDescription()).append(" in license"));
			builder.add(UsageTermLicenseUsageName.class, e -> c -> c.append(e.getTerm().getDescription()).append(" in usage"));
		}
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final IRendering<ILicenseUsageName, ILicenseUsageNameRenderContext, IExplicitRenderable<? super ILicenseUsageNameRenderContext>> renderingStatic = new LicenseUsageNameRendering();

	protected final IContext context;

	@Override
	protected ILicenseUsageNameRenderContext createContext(TextNestedModifiedBuilder builder) {
		return new LicenseUsageNameRenderContext(builder, getContext());
	}

	@Override
	protected IRendering<? super ILicenseUsageName, ? extends ILicenseUsageNameRenderContext, ? extends IExplicitRenderable<? super ILicenseUsageNameRenderContext>> getRendering() {
		return getRenderingStatic();
	}
}
