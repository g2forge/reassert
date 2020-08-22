package com.g2forge.reassert.contract.eee.explain.convert;

import java.util.List;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.IConsumer2;
import com.g2forge.alexandria.java.nestedstate.StackGlobalState;
import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.enigma.backend.convert.ARenderer;
import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.enigma.backend.convert.IRendering;
import com.g2forge.enigma.backend.convert.textual.ATextualRenderer;
import com.g2forge.enigma.backend.text.model.modifier.TextNestedModified;
import com.g2forge.reassert.contract.eee.explain.model.IExplained;
import com.g2forge.reassert.contract.eee.explain.model.IExplainedApplication;
import com.g2forge.reassert.contract.eee.explain.model.IExplainedApplication.Argument;
import com.g2forge.reassert.contract.eee.explain.model.IdentityExplainedOperation;
import com.g2forge.reassert.contract.eee.explain.model.ZeroExplainedOperation;
import com.g2forge.reassert.contract.eee.express.IConstant;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ExplanationRenderer extends ATextualRenderer<IExplained<?>, IExplanationRenderContext> {
	@Getter(AccessLevel.PROTECTED)
	protected class ExplanationRenderContext extends ARenderContext implements IExplanationRenderContext {
		protected final StackGlobalState<ExplanationMode> state;

		protected final IConsumer2<TextNestedModified.TextNestedModifiedBuilder, Object> valueContext;

		public ExplanationRenderContext(TextNestedModified.TextNestedModifiedBuilder builder, ExplanationMode mode, IConsumer2<TextNestedModified.TextNestedModifiedBuilder, Object> valueContext) {
			super(builder);
			this.state = new StackGlobalState<ExplanationMode>(mode);
			this.valueContext = valueContext;
		}

		@Override
		public ExplanationMode getMode() {
			return getState().get();
		}

		@Override
		protected IExplanationRenderContext getThis() {
			return this;
		}

		@Override
		public IExplanationRenderContext value(Object value) {
			valueContext.accept(getBuilder(), value);
			return getThis();
		}
	}

	protected static class ExplanationRendering extends ARenderer.ARendering<IExplained<?>, IExplanationRenderContext, IExplicitRenderable<? super IExplanationRenderContext>> {
		@Override
		protected void extend(TypeSwitch1.FunctionBuilder<IExplained<?>, IExplicitRenderable<? super IExplanationRenderContext>> builder) {
			builder.add(IExplicitExplanationRenderable.class, e -> c -> ((IExplicitExplanationRenderable<?>) e).render(c));
			builder.add(IConstant.class, e -> c -> {
				final IConstant<?> castE = (IConstant<?>) e;
				c.value(castE.get());
				final String name = castE.getName();
				if (name != null) c.append(" - ").append(name);
			});
			builder.add(ZeroExplainedOperation.class, e -> c -> {
				c.value(e.get()).append(" (").append(e.getOperator()).append(")").append(" - because ");
				if (c.getMode().compareTo(ExplanationMode.Describe) <= 0) {
					final ZeroExplainedOperation.Argument example = ((ZeroExplainedOperation<?>) e).getArguments().stream().filter(ZeroExplainedOperation.Argument::isRelevant).findAny().get();
					c.append("one or more arguments were ").value(e.getZero()).append(", for example: ").render(example.getArgument(), IExplained.class);
				} else {
					c.append(e.getZero()).append(" is the zero of the ").append(e.getOperator()).append(" operator");
					final List<ZeroExplainedOperation.Argument> arguments = ((ZeroExplainedOperation<?>) e).getArguments();
					if ((arguments != null) && !arguments.isEmpty()) {
						final List<Argument> printableArguments = arguments.stream().filter(argument -> !isSkipped(c, argument)).collect(Collectors.toList());
						if (!printableArguments.isEmpty()) {
							c.newline();
							try (final ICloseable indent = c.indent()) {
								boolean first = true;
								for (IExplainedApplication.Argument argument : printableArguments) {
									if (first) first = false;
									else c.newline();
									c.append(argument.isRelevant() ? "* " : "_ ").render(argument.getArgument(), IExplained.class);
								}
							}
						}
					}
				}
			});
			builder.add(IdentityExplainedOperation.class, e -> c -> {
				c.value(e.get()).append(" (").append(e.getOperator()).append(")").append(" - because ");
				final List<IdentityExplainedOperation.Argument> arguments = ((IdentityExplainedOperation<?>) e).getArguments();
				if ((c.getMode().compareTo(ExplanationMode.Describe) <= 0) && (arguments.stream().filter(IdentityExplainedOperation.Argument::isRelevant).collect(Collectors.counting()) == 1)) {
					c.append("there's a single relevant argument: ").render(arguments.get(0).getArgument(), IExplained.class);
				} else {
					final boolean printArguments;
					if (arguments.size() == 1) {
						c.append("there's a single argument");
						printArguments = c.getMode().compareTo(ExplanationMode.Trace) >= 0;
						if (!printArguments) c.append(": ").render(HCollection.getOne(arguments).getArgument(), IExplained.class);
					} else {
						c.append("the ").append(e.getOperator()).append(" operator (with identity ").value(e.getIdentity()).append(") was applied");
						printArguments = true;
					}

					if (printArguments && (arguments != null) && !arguments.isEmpty()) {
						final List<Argument> printableArguments = arguments.stream().filter(argument -> !isSkipped(c, argument)).collect(Collectors.toList());
						if (!printableArguments.isEmpty()) {
							c.newline();
							try (final ICloseable indent = c.indent()) {
								boolean first = true;
								for (IExplainedApplication.Argument argument : printableArguments) {
									if (first) first = false;
									else c.newline();
									c.append(argument.isRelevant() ? "* " : "_ ").render(argument.getArgument(), IExplained.class);
								}
							}
						}
					}
				}
			});
		}

		protected boolean isSkipped(IExplanationRenderContext context, IExplainedApplication.Argument argument) {
			return !argument.isRelevant() && (context.getMode().compareTo(ExplanationMode.Explain) <= 0);
		}
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final IRendering<IExplained<?>, IExplanationRenderContext, IExplicitRenderable<? super IExplanationRenderContext>> renderingStatic = new ExplanationRendering();

	protected final ExplanationMode mode;

	protected final IConsumer2<TextNestedModified.TextNestedModifiedBuilder, Object> valueContext;

	public ExplanationRenderer() {
		this(ExplanationMode.Explain);
	}

	public ExplanationRenderer(ExplanationMode mode) {
		this(mode, (builder, value) -> builder.expression(value));
	}

	@Override
	protected ExplanationRenderContext createContext(TextNestedModified.TextNestedModifiedBuilder builder) {
		return new ExplanationRenderContext(builder, getMode(), getValueContext());
	}

	@Override
	protected IRendering<? super IExplained<?>, ? extends IExplanationRenderContext, ? extends IExplicitRenderable<? super IExplanationRenderContext>> getRendering() {
		return getRenderingStatic();
	}
}
