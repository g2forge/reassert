package com.g2forge.reassert.contract.algorithm.worklicense.convert;

import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.enigma.backend.convert.ARenderer;
import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.enigma.backend.convert.IRendering;
import com.g2forge.enigma.backend.convert.textual.ATextualRenderer;
import com.g2forge.enigma.backend.text.model.modifier.TextNestedModified.TextNestedModifiedBuilder;
import com.g2forge.reassert.contract.algorithm.worklicense.model.name.BaseWorkLicenseName;
import com.g2forge.reassert.contract.algorithm.worklicense.model.name.IWorkLicenseName;
import com.g2forge.reassert.contract.algorithm.worklicense.model.name.LicenseWorkLicenseName;
import com.g2forge.reassert.contract.algorithm.worklicense.model.name.TestWorkLicenseName;
import com.g2forge.reassert.core.api.described.IDescription;
import com.g2forge.reassert.core.api.module.IContext;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class WorkLicenseNameRenderer extends ATextualRenderer<IWorkLicenseName, IWorkLicenseNameRenderContext> {
	@Getter
	protected class WorkLicenseNameRenderContext extends ARenderContext implements IWorkLicenseNameRenderContext {
		protected final IContext context;

		public WorkLicenseNameRenderContext(TextNestedModifiedBuilder builder, IContext context) {
			super(builder);
			this.context = context;
		}

		@Override
		protected IWorkLicenseNameRenderContext getThis() {
			return this;
		}
	}

	protected static class WorkLicenseNameRendering extends ARenderer.ARendering<IWorkLicenseName, IWorkLicenseNameRenderContext, IExplicitRenderable<? super IWorkLicenseNameRenderContext>> {
		@Override
		protected void extend(TypeSwitch1.FunctionBuilder<IWorkLicenseName, IExplicitRenderable<? super IWorkLicenseNameRenderContext>> builder) {
			builder.add(LicenseWorkLicenseName.class, e -> c -> {
				c.append(e.getTerm().getDescription()).append(" in ");
				final IDescription contract = c.getContext().describe(e.getLicense());
				c.append(contract.getName());
			});
			builder.add(BaseWorkLicenseName.class, e -> c -> c.append(e.getTerm().getDescription()).append(" in work license"));
			builder.add(TestWorkLicenseName.class, e -> c -> c.append(e.getTerm().getDescription()).append(" in other license"));
		}
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final IRendering<IWorkLicenseName, IWorkLicenseNameRenderContext, IExplicitRenderable<? super IWorkLicenseNameRenderContext>> renderingStatic = new WorkLicenseNameRendering();

	protected final IContext context;

	@Override
	protected IWorkLicenseNameRenderContext createContext(TextNestedModifiedBuilder builder) {
		return new WorkLicenseNameRenderContext(builder, getContext());
	}

	@Override
	protected IRendering<? super IWorkLicenseName, ? extends IWorkLicenseNameRenderContext, ? extends IExplicitRenderable<? super IWorkLicenseNameRenderContext>> getRendering() {
		return getRenderingStatic();
	}
}
