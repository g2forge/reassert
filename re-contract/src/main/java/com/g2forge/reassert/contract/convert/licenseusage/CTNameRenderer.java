package com.g2forge.reassert.contract.convert.licenseusage;

import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.enigma.backend.convert.ARenderer;
import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.enigma.backend.convert.IRendering;
import com.g2forge.enigma.backend.convert.textual.ATextualRenderer;
import com.g2forge.enigma.backend.text.model.modifier.TextNestedModified.TextNestedModifiedBuilder;
import com.g2forge.reassert.contract.model.licenseusage.CTNameContract;
import com.g2forge.reassert.contract.model.licenseusage.CTNameLicenseTerm;
import com.g2forge.reassert.contract.model.licenseusage.CTNameUsageTerm;
import com.g2forge.reassert.contract.model.licenseusage.ICTName;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.module.IContext;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class CTNameRenderer extends ATextualRenderer<ICTName, ICTNameRenderContext> {
	@Getter
	protected class CTNameRenderContext extends ARenderContext implements ICTNameRenderContext {
		protected final IContext context;

		public CTNameRenderContext(TextNestedModifiedBuilder builder, IContext context) {
			super(builder);
			this.context = context;
		}

		@Override
		protected ICTNameRenderContext getThis() {
			return this;
		}
	}

	protected static class CTNameRendering extends ARenderer.ARendering<ICTName, ICTNameRenderContext, IExplicitRenderable<? super ICTNameRenderContext>> {
		@Override
		protected void extend(TypeSwitch1.FunctionBuilder<ICTName, IExplicitRenderable<? super ICTNameRenderContext>> builder) {
			builder.add(CTNameContract.class, e -> c -> {
				c.append(e.getTerm().getDescription()).append(" in ");
				final IDescription contract = c.getContext().describe(e.getContract());
				c.append(contract.getName());
			});
			builder.add(CTNameLicenseTerm.class, e -> c -> c.append(e.getTerm().getDescription()).append(" in license"));
			builder.add(CTNameUsageTerm.class, e -> c -> c.append(e.getTerm().getDescription()).append(" in usage"));
		}
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final IRendering<ICTName, ICTNameRenderContext, IExplicitRenderable<? super ICTNameRenderContext>> renderingStatic = new CTNameRendering();

	protected final IContext context;

	@Override
	protected ICTNameRenderContext createContext(TextNestedModifiedBuilder builder) {
		return new CTNameRenderContext(builder, getContext());
	}

	@Override
	protected IRendering<? super ICTName, ? extends ICTNameRenderContext, ? extends IExplicitRenderable<? super ICTNameRenderContext>> getRendering() {
		return getRenderingStatic();
	}
}
