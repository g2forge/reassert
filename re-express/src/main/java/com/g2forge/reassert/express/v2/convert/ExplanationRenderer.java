package com.g2forge.reassert.express.v2.convert;

import java.util.Collection;
import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.fluent.optional.IOptional;
import com.g2forge.alexandria.java.fluent.optional.NullableOptional;
import com.g2forge.alexandria.java.nestedstate.StackGlobalState;
import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.enigma.backend.convert.ARenderer;
import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.enigma.backend.convert.IRendering;
import com.g2forge.enigma.backend.convert.textual.ATextualRenderer;
import com.g2forge.enigma.backend.convert.textual.ITextualRenderer;
import com.g2forge.enigma.backend.convert.textual.ToStringTextualRenderer;
import com.g2forge.enigma.backend.text.model.modifier.TextNestedModified;
import com.g2forge.reassert.express.v2.eval.operation.IOperationSystem;
import com.g2forge.reassert.express.v2.eval.operation.IOperatorRendering;
import com.g2forge.reassert.express.v2.eval.value.IValueSystem;
import com.g2forge.reassert.express.v2.model.IExplained;
import com.g2forge.reassert.express.v2.model.IExplained.Relevance;
import com.g2forge.reassert.express.v2.model.constant.ILiteral;
import com.g2forge.reassert.express.v2.model.operation.ExplainedOperation;
import com.g2forge.reassert.express.v2.model.operation.IExplainedOperation;
import com.g2forge.reassert.express.v2.model.operation.ZeroExplainedOperation;
import com.g2forge.reassert.express.v2.model.variable.IExplainedClosure;
import com.g2forge.reassert.express.v2.model.variable.IExplainedClosure.Binding;
import com.g2forge.reassert.express.v2.model.variable.IExplainedVariable;
import com.g2forge.reassert.express.v2.model.variable.IVariable;

import lombok.AccessLevel;
import lombok.Builder;
import lombok.Data;
import lombok.EqualsAndHashCode;
import lombok.Getter;
import lombok.RequiredArgsConstructor;
import lombok.ToString;

@Getter
@RequiredArgsConstructor
public class ExplanationRenderer<Name, Value> extends ATextualRenderer<IExplained<Value>, IExplanationRenderContext<Name, Value>> {
	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	protected static class Child<Name, Value> implements IExplained<Value> {
		protected final ExplanationMode mode;

		protected final IExplained.Relevance relevance;

		protected final IVariable<Name, Value> variable;

		protected final IOptional<? extends IExplained<Value>> value;

		@Getter(lazy = true)
		@EqualsAndHashCode.Exclude
		@ToString.Exclude
		private final String prefix = computePrefix();

		public Child(ExplanationMode mode, IExplainedClosure.Binding<Name, Value> binding) {
			this(mode, binding.getRelevance(), binding.getVariable(), binding.getExplained());
		}

		public Child(ExplanationMode mode, IExplainedOperation.Argument<Value> argument) {
			this(mode, argument.getRelevance(), null, NullableOptional.of(argument.getExplained()));
		}

		protected String computePrefix() {
			final Relevance relevance = getRelevance();
			switch (relevance) {
				case Unevaluated:
					return " ";
				case Identity:
					return "-";
				case Combined:
					return "*";
				case Dominant:
					return ">";
				default:
					throw new EnumException(IExplained.Relevance.class, relevance);
			}
		}

		@Override
		public Value get() {
			return getValue().get().get();
		}

		public boolean isPrintable() {
			final Relevance relevance = getRelevance();
			switch (relevance) {
				case Unevaluated:
				case Identity:
					return getMode().compareTo(ExplanationMode.Trace) >= 0;
				case Combined:
				case Dominant:
					return true;
				default:
					throw new EnumException(IExplained.Relevance.class, relevance);
			}
		}
	}

	@Getter(AccessLevel.PROTECTED)
	protected class ExplanationRenderContext extends ARenderContext implements IExplanationRenderContext<Name, Value> {
		protected final StackGlobalState<ExplanationMode> state;

		protected final IValueSystem<? super Value> valueSystem;

		protected final ITextualRenderer<? super Name> nameRenderer;

		public ExplanationRenderContext(TextNestedModified.TextNestedModifiedBuilder builder, ExplanationMode mode, IValueSystem<? super Value> valueSystem, ITextualRenderer<? super Name> nameRenderer) {
			super(builder);
			this.state = new StackGlobalState<ExplanationMode>(mode);
			this.valueSystem = valueSystem;
			this.nameRenderer = nameRenderer;
		}

		@Override
		public ExplanationMode getMode() {
			return getState().get();
		}

		@Override
		protected IExplanationRenderContext<Name, Value> getThis() {
			return this;
		}

		@Override
		public IExplanationRenderContext<Name, Value> name(Name name) {
			getNameRenderer().render(getBuilder(), name);
			return getThis();
		}

		@Override
		public IExplanationRenderContext<Name, Value> value(Value value) {
			getValueSystem().getRenderer().render(getBuilder(), value);
			return getThis();
		}
	}

	@Getter(AccessLevel.PROTECTED)
	@RequiredArgsConstructor
	protected static class ExplanationRendering<Name, Value> extends ARenderer.ARendering<IExplained<Value>, IExplanationRenderContext<Name, Value>, IExplicitRenderable<? super IExplanationRenderContext<Name, Value>>> {
		protected final IOperationSystem<Value> operationSystem;

		@Override
		protected void extend(TypeSwitch1.FunctionBuilder<IExplained<Value>, IExplicitRenderable<? super IExplanationRenderContext<Name, Value>>> builder) {
			builder.add(IExplicitExplanationRenderable.class, e -> c -> {
				@SuppressWarnings("unchecked")
				final IExplicitExplanationRenderable<Name, Value> expression = (IExplicitExplanationRenderable<Name, Value>) e;
				expression.render(c);
			});
			builder.add(ILiteral.class, e -> c -> {
				@SuppressWarnings("unchecked")
				final ILiteral<Name, Value> explained = (ILiteral<Name, Value>) e;

				c.value(explained.get());
				final Name name = explained.getName();
				if (name != null) c.append(" - ").name(name);
			});

			builder.add(IExplainedVariable.class, e -> c -> {
				@SuppressWarnings("unchecked")
				final IExplainedVariable<Name, Value> explained = (IExplainedVariable<Name, Value>) e;
				final IExplanationRenderContext<Name, Value> context = (IExplanationRenderContext<Name, Value>) c;

				context.name(explained.getVariable().getName());
				final IOptional<? extends IExplained<Value>> nested = explained.getExplained();
				if (nested.isNotEmpty()) context.append(" = ").render(nested.get(), IExplained.class);
			});
			builder.add(IExplainedClosure.class, e -> c -> {
				@SuppressWarnings("unchecked")
				final IExplainedClosure<Name, Value> explained = (IExplainedClosure<Name, Value>) e;
				final IExplanationRenderContext<Name, Value> context = (IExplanationRenderContext<Name, Value>) c;

				if (context.getMode().compareTo(ExplanationMode.Describe) <= 0) context.render(explained.getExpression(), IExplained.class);
				else {
					context.value(explained.get()).append(" - result closure application");
					if (explained.getBindings().isEmpty()) context.append(", ").render(explained.getExpression(), IExplained.class);
					else {
						context.newline();
						try (final ICloseable indent = context.indent()) {
							for (Binding<Name, Value> binding : explained.getBindings()) {
								context.render(new Child<>(context.getMode(), binding), Child.class).newline();
							}
							context.render(explained.getExpression(), IExplained.class);
						}
					}
				}
			});

			builder.add(Child.class, e -> c -> {
				@SuppressWarnings("unchecked")
				final Child<Name, Value> explained = (Child<Name, Value>) e;
				final IExplanationRenderContext<Name, Value> context = (IExplanationRenderContext<Name, Value>) c;

				context.append(explained.getPrefix()).append(' ');

				final boolean hasVariable = explained.getVariable() != null;
				final boolean hasValue = explained.getValue().isNotEmpty();
				if (hasVariable) {
					context.name(explained.getVariable().getName());
					if (hasValue) context.append(" = ");
				}
				if (hasValue) context.render(explained.getValue().get(), IExplained.class);
			});
			builder.add(ZeroExplainedOperation.class, e -> c -> {
				@SuppressWarnings("unchecked")
				final ZeroExplainedOperation<Value> explained = (ZeroExplainedOperation<Value>) e;
				final IExplanationRenderContext<Name, Value> context = (IExplanationRenderContext<Name, Value>) c;

				context.value(explained.get()).append(" - because anything ").append(getOperationSystem().getRendering(explained.getOperator()).getPastVerb()).append(" ").value(explained.getZero()).append(" is ").value(explained.getZero());
				if (context.getMode().compareTo(ExplanationMode.Describe) <= 0) {
					context.append(", and one argument was ");
					context.render(explained.getArguments().stream().filter(a -> Relevance.Dominant.equals(a.getRelevance())).findFirst().get().getExplained(), IExplained.class);
				} else {
					final List<IExplainedOperation.Argument<Value>> arguments = explained.getArguments();
					if ((arguments != null) && !arguments.isEmpty()) print(context, arguments.stream().map(a -> new Child<Name, Value>(context.getMode(), a)).filter(Child::isPrintable).collect(Collectors.toList()));
				}
			});
			builder.add(ExplainedOperation.class, e -> c -> {
				@SuppressWarnings("unchecked")
				final ExplainedOperation<Value> explained = (ExplainedOperation<Value>) e;
				final IExplanationRenderContext<Name, Value> context = (IExplanationRenderContext<Name, Value>) c;

				context.value(explained.get()).append(" - ");

				final IOperatorRendering operatorRendering = getOperationSystem().getRendering(explained.getOperator());
				final List<IExplainedOperation.Argument<Value>> arguments = explained.getArguments() == null ? Collections.emptyList() : explained.getArguments();

				final Map<Relevance, List<IExplainedOperation.Argument<Value>>> argumentsByRelevance = arguments.stream().collect(Collectors.groupingBy(IExplainedOperation.Argument::getRelevance));
				if (argumentsByRelevance.get(IExplained.Relevance.Unevaluated) != null) throw new IllegalArgumentException("Without a zero, there shouldn't be any unevaluated arguments!");

				if (explained.getIdentity().isNotEmpty() && (argumentsByRelevance.containsKey(Relevance.Identity) || arguments.isEmpty())) {
					context.append("because anything ").append(operatorRendering.getPastVerb()).append(" ").value(explained.getIdentity().get()).append(" is itself");
					if (arguments.isEmpty()) context.append(", and there were no arguments");
				} else context.append(operatorRendering.getName());

				final List<Child<Name, Value>> childrenAll = arguments.stream().map(a -> new Child<Name, Value>(context.getMode(), a)).collect(Collectors.toList());
				final List<Child<Name, Value>> childrenPrintable = childrenAll.stream().filter(Child::isPrintable).collect(Collectors.toList());
				if ((context.getMode().compareTo(ExplanationMode.Describe) <= 0) && ((childrenAll.size() == 1) || (childrenPrintable.size() == 1))) {
					final List<Child<Name, Value>> toPrint = (childrenAll.size() == 1) ? childrenAll : childrenPrintable;
					context.append(", and the").append(toPrint.size() != arguments.size() ? " relevant" : " only").append(" argument is ");
					context.render(HCollection.getOne(toPrint).getValue().get(), IExplained.class);
				} else print(context, childrenPrintable.isEmpty() ? childrenAll : childrenPrintable);
			});
		}

		protected void print(final IExplanationRenderContext<Name, Value> context, final List<Child<Name, Value>> children) {
			if (!children.isEmpty()) {
				context.newline();
				try (final ICloseable indent = context.indent()) {
					boolean first = true;
					for (Child<Name, Value> argument : children) {
						if (first) first = false;
						else context.newline();
						context.render(argument, Child.class);
					}
				}
			}
		}
	}

	protected final ExplanationMode mode;

	protected final ITextualRenderer<? super Name> nameRenderer;

	protected final IValueSystem<? super Value> valueSystem;

	protected final IOperationSystem<Value> operationSystem;

	public ExplanationRenderer(ExplanationMode mode, IValueSystem<? super Value> valueSystem, IOperationSystem<Value> operationSystem) {
		this(mode, ToStringTextualRenderer.create(), valueSystem, operationSystem);
	}

	@Override
	protected ExplanationRenderContext createContext(TextNestedModified.TextNestedModifiedBuilder builder) {
		return new ExplanationRenderContext(builder, getMode(), getValueSystem(), getNameRenderer());
	}

	@Override
	protected IRendering<? super IExplained<Value>, ? extends IExplanationRenderContext<Name, Value>, ? extends IExplicitRenderable<? super IExplanationRenderContext<Name, Value>>> getRendering() {
		return new ExplanationRendering<>(getOperationSystem());
	}
}
