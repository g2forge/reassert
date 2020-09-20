package com.g2forge.reassert.express.v2.convert;

import java.util.Collections;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.helpers.HCollection;
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
				final ILiteral<Name, Value> expression = (ILiteral<Name, Value>) e;

				c.value(expression.get());
				final Name name = expression.getName();
				if (name != null) c.append(" - ").name(name);
			});
			builder.add(PrintableArgument.class, e -> c -> {
				@SuppressWarnings("unchecked")
				final PrintableArgument<Value> expression = (PrintableArgument<Value>) e;
				final IExplanationRenderContext<Name, Value> context = (IExplanationRenderContext<Name, Value>) c;

				context.append(expression.getPrefix()).append(' ').render(expression.getArgument().getExplained(), IExplained.class);
			});
			builder.add(ZeroExplainedOperation.class, e -> c1 -> {
				@SuppressWarnings("unchecked")
				final ZeroExplainedOperation<Value> expression = (ZeroExplainedOperation<Value>) e;
				final IExplanationRenderContext<Name, Value> context = (IExplanationRenderContext<Name, Value>) c1;

				context.value(expression.get()).append(" - because anything ").append(getOperationSystem().getRendering(expression.getOperator()).getPastVerb()).append(" ").value(expression.getZero()).append(" is ").value(expression.getZero());
				if (context.getMode().compareTo(ExplanationMode.Describe) <= 0) {
					context.append(", and one argument was ");
					context.render(expression.getArguments().stream().filter(a -> Relevance.Dominant.equals(a.getRelevance())).findFirst().get().getExplained(), IExplained.class);
				} else {
					final List<IExplainedOperation.Argument<Value>> arguments = expression.getArguments();
					if ((arguments != null) && !arguments.isEmpty()) printArguments(context, arguments.stream().map(a -> new PrintableArgument<>(context.getMode(), a)).filter(PrintableArgument::isPrintable).collect(Collectors.toList()));
				}
			});
			builder.add(ExplainedOperation.class, e -> c -> {
				@SuppressWarnings("unchecked")
				final ExplainedOperation<Value> expression = (ExplainedOperation<Value>) e;
				final IExplanationRenderContext<Name, Value> context = (IExplanationRenderContext<Name, Value>) c;

				context.value(expression.get()).append(" - ");

				final IOperatorRendering operatorRendering = getOperationSystem().getRendering(expression.getOperator());
				final List<IExplainedOperation.Argument<Value>> arguments = expression.getArguments() == null ? Collections.emptyList() : expression.getArguments();

				final Map<Relevance, List<IExplainedOperation.Argument<Value>>> argumentsByRelevance = arguments.stream().collect(Collectors.groupingBy(IExplainedOperation.Argument::getRelevance));
				if (argumentsByRelevance.get(IExplained.Relevance.Unevaluated) != null) throw new IllegalArgumentException("Without a zero, there shouldn't be any unevaluated arguments!");

				if (expression.getIdentity().isNotEmpty() && (argumentsByRelevance.containsKey(Relevance.Identity) || arguments.isEmpty())) {
					context.append("because anything ").append(operatorRendering.getPastVerb()).append(" ").value(expression.getIdentity().get()).append(" is itself");
					if (arguments.isEmpty()) context.append(", and there were no arguments");
				} else context.append(operatorRendering.getName());

				final List<PrintableArgument<Value>> printableArgumentsAll = arguments.stream().map(a -> new PrintableArgument<>(context.getMode(), a)).collect(Collectors.toList());
				final List<PrintableArgument<Value>> printableArgumentsPrintable = printableArgumentsAll.stream().filter(PrintableArgument::isPrintable).collect(Collectors.toList());
				if ((context.getMode().compareTo(ExplanationMode.Describe) <= 0) && ((printableArgumentsAll.size() == 1) || (printableArgumentsPrintable.size() == 1))) {
					final List<PrintableArgument<Value>> toPrint = (printableArgumentsAll.size() == 1) ? printableArgumentsAll : printableArgumentsPrintable;
					context.append(", and the").append(toPrint.size() != arguments.size() ? " relevant" : " only").append(" argument is ");
					context.render(HCollection.getOne(toPrint).getArgument().getExplained(), IExplained.class);
				} else {
					final List<PrintableArgument<Value>> toPrint = printableArgumentsPrintable.isEmpty() ? printableArgumentsAll : printableArgumentsPrintable;
					printArguments(context, toPrint);
				}
			});
		}

		protected void printArguments(final IExplanationRenderContext<Name, Value> context, final List<PrintableArgument<Value>> printableArguments) {
			if (!printableArguments.isEmpty()) {
				context.newline();
				try (final ICloseable indent = context.indent()) {
					boolean first = true;
					for (PrintableArgument<Value> argument : printableArguments) {
						if (first) first = false;
						else context.newline();
						context.render(argument, PrintableArgument.class);
					}
				}
			}
		}
	}

	@Data
	@Builder(toBuilder = true)
	@RequiredArgsConstructor
	protected static class PrintableArgument<Value> implements IExplained<Value> {
		protected final ExplanationMode mode;

		protected final IExplainedOperation.Argument<Value> argument;

		@Getter(lazy = true)
		@EqualsAndHashCode.Exclude
		@ToString.Exclude
		private final String prefix = computePrefix();

		protected String computePrefix() {
			final Relevance relevance = getArgument().getRelevance();
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
			return getArgument().getExplained().get();
		}

		public boolean isPrintable() {
			final Relevance relevance = getArgument().getRelevance();
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
