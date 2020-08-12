package com.g2forge.reassert.term.analyze.convert;

import java.util.ArrayList;
import java.util.Collection;
import java.util.List;

import org.slf4j.event.Level;

import com.g2forge.alexandria.java.close.ICloseable;
import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.function.IConsumer2;
import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.enigma.backend.convert.ARenderer;
import com.g2forge.enigma.backend.convert.IExplicitRenderable;
import com.g2forge.enigma.backend.convert.IRendering;
import com.g2forge.enigma.backend.convert.textual.ATextualRenderer;
import com.g2forge.enigma.backend.text.model.modifier.TextNestedModified;
import com.g2forge.reassert.core.model.contract.ITerm;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.usage.IUsageTerm;
import com.g2forge.reassert.core.model.report.IFinding;
import com.g2forge.reassert.core.model.report.IReport;
import com.g2forge.reassert.term.analyze.model.IExpressionContext;
import com.g2forge.reassert.term.analyze.model.TermConstant;
import com.g2forge.reassert.term.analyze.model.TermType;
import com.g2forge.reassert.term.analyze.model.findings.ConditionFinding;
import com.g2forge.reassert.term.analyze.model.findings.ExpressionContextualFinding;
import com.g2forge.reassert.term.analyze.model.findings.IRiskFinding;
import com.g2forge.reassert.term.analyze.model.findings.UnrecognizedTermFinding;
import com.g2forge.reassert.term.eee.explain.convert.ExplanationMode;
import com.g2forge.reassert.term.eee.explain.convert.ExplanationRenderer;
import com.g2forge.reassert.term.eee.explain.model.IExplained;
import com.g2forge.reassert.term.eee.express.IExpression;
import com.g2forge.reassert.term.eee.express.IConstant;
import com.g2forge.reassert.term.eee.express.Operation;

import lombok.AccessLevel;
import lombok.Getter;
import lombok.RequiredArgsConstructor;

@Getter
@RequiredArgsConstructor
public class ReportRenderer extends ATextualRenderer<Object, IReportRenderContext> {
	@Getter(AccessLevel.PROTECTED)
	protected class ReportRenderContext extends ARenderContext implements IReportRenderContext {
		@Getter(AccessLevel.PUBLIC)
		protected final ExplanationMode mode;

		protected final ExplanationRenderer explanationRenderer;

		protected final IConsumer2<TextNestedModified.TextNestedModifiedBuilder, Object> valueContext;

		@Getter(AccessLevel.PUBLIC)
		protected IExpressionContext findingContext;

		public ReportRenderContext(TextNestedModified.TextNestedModifiedBuilder builder, ExplanationMode mode, IConsumer2<TextNestedModified.TextNestedModifiedBuilder, Object> valueContext) {
			super(builder);
			this.mode = mode;
			this.explanationRenderer = new ExplanationRenderer(getMode());
			this.valueContext = valueContext;
		}

		@Override
		public ICloseable findingContext(IExpressionContext findingContext) {
			final IExpressionContext previous = this.findingContext;
			this.findingContext = findingContext;
			return () -> this.findingContext = previous;
		}

		@Override
		protected IReportRenderContext getThis() {
			return this;
		}

		@Override
		public IReportRenderContext render(IExplained<?> explained) {
			explanationRenderer.render(getBuilder(), explained);
			return this;
		}
	}

	protected static class ReportRendering extends ARenderer.ARendering<Object, IReportRenderContext, IExplicitRenderable<? super IReportRenderContext>> {
		@Override
		protected void extend(TypeSwitch1.FunctionBuilder<Object, IExplicitRenderable<? super IReportRenderContext>> builder) {
			builder.add(IExplicitReportRenderable.class, e -> c -> ((IExplicitReportRenderable<?>) e).render(c));
			builder.add(IExplained.class, e -> c -> c.render(e));

			builder.add(IReport.class, e -> c -> {
				c.append("Minimum finding level: ");
				final Collection<IFinding> findings = e.getFindings();
				if (findings.isEmpty()) c.append("NONE").newline();
				else {
					c.append(e.getMinLevel()).newline();
					if (findings != null) for (IFinding finding : findings) {
						c.render(finding, IFinding.class).newline();
					}
				}
			});

			builder.add(ILicenseTerm.class, e -> c -> c.append('\"').append(e.getDescription()).append('\"'));
			builder.add(IUsageTerm.class, e -> c -> c.append('\"').append(e.getDescription()).append('\"'));

			builder.add(IConstant.class, e -> c -> c.append('(').append(e.getName()).append(')'));
			builder.add(TermConstant.class, e -> c -> c.render(e.getTerm(), ITerm.class));
			builder.add(Operation.class, e -> c -> {
				final Operation<?> castE = (Operation<?>) e;
				c.append('(');
				final String separator;
				switch (castE.getOperator()) {
					case NOT:
						c.append('!').render(HCollection.getOne(castE.getArguments()), IExpression.class).append(')');
						return;
					case AND:
						separator = "&&";
						break;
					case OR:
						separator = "||";
						break;
					default:
						throw new EnumException(Operation.Operator.class, castE.getOperator());
				}

				boolean first = true;
				for (IExpression<?> argument : castE.getArguments()) {
					if (first) first = false;
					else c.append(' ').append(separator).append(' ');
					c.render(argument, IExpression.class);
				}
				c.append(')');
			});

			builder.add(UnrecognizedTermFinding.class, e -> c -> {
				final TermType termType = TermType.valueOf(e.getTerm());
				c.append(e.getLevel()).render(e.getTerm(), ITerm.class).append(" is not recognized so we cannot analyze ");
				switch (termType) {
					case License:
						c.append("whether the condition has been met.");
						break;
					case Usage:
						c.append("whether the licenses allow this usage.");
						break;
					default:
						throw new EnumException(TermType.class, termType);
				}
			});
			builder.add(ExpressionContextualFinding.class, e -> c -> {
				try (final ICloseable findingContext = c.findingContext(e)) {
					c.render(e.getFinding(), IFinding.class);
				}
			});
			builder.add(ConditionFinding.class, e -> c -> {
				final IExpressionContext findingContext = c.getFindingContext();
				final Collection<ITerm> outputs = findingContext == null ? HCollection.emptyList() : findingContext.getOutputs();

				c.append(e.getLevel()).append(": Condition");
				if (outputs.size() > 1) c.append('s');
				c.append(' ');
				if (findingContext != null) {
					final List<ITerm> list = new ArrayList<>(outputs);
					final int size = list.size();
					for (int i = 0; i < size; i++) {
						if (i > 0) c.append((i == size - 1) ? " & " : ", ");
						c.render(list.get(i), ITerm.class);
					}
				}

				c.append(' ').append(outputs.size() > 1 ? "are" : "is");
				if (!e.isSatisfied()) c.append(" not");
				c.append(" satisfied");

				if (findingContext != null) {
					c.append(" based on ");
					final List<ITerm> list = new ArrayList<>(findingContext.getInputs());
					final int size = list.size();
					for (int i = 0; i < size; i++) {
						if (i > 0) c.append((i == size - 1) ? " & " : ", ");
						c.render(list.get(i), ITerm.class);
					}
				}

				if ((!e.isSatisfied()) || (c.getMode().compareTo(ExplanationMode.Explain) >= 0)) try (final ICloseable indent = c.newline().indent()) {
					if (findingContext != null) c.append("Rule: ").render(findingContext.getExpression(), IExpression.class).newline();
					c.append("Explanation: ").render(e.getResult());
				}
			});
			builder.add(IRiskFinding.class, e -> c -> {
				c.append(e.getLevel()).append(": ").append(e.getDescription());
				if ((e.getLevel().compareTo(Level.WARN) <= 0) || (c.getMode().compareTo(ExplanationMode.Explain) >= 0)) try (final ICloseable indent = c.newline().indent()) {
					final IExpressionContext findingContext = c.getFindingContext();
					if (findingContext != null) c.append("Rule: ").render(findingContext.getExpression(), IExpression.class).newline();
					c.append("Explanation: ").render(e.getResult());
				}
			});
		}
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private static final IRendering<Object, IReportRenderContext, IExplicitRenderable<? super IReportRenderContext>> renderingStatic = new ReportRendering();

	protected final ExplanationMode mode;

	protected final IConsumer2<TextNestedModified.TextNestedModifiedBuilder, Object> valueContext;

	public ReportRenderer() {
		this(ExplanationMode.Explain);
	}

	public ReportRenderer(ExplanationMode mode) {
		this(mode, (builder, value) -> builder.expression(value));
	}

	@Override
	protected ReportRenderContext createContext(TextNestedModified.TextNestedModifiedBuilder builder) {
		return new ReportRenderContext(builder, getMode(), getValueContext());
	}

	@Override
	protected IRendering<? super Object, ? extends IReportRenderContext, ? extends IExplicitRenderable<? super IReportRenderContext>> getRendering() {
		return getRenderingStatic();
	}
}
