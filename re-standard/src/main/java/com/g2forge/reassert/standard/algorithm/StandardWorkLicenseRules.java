package com.g2forge.reassert.standard.algorithm;

import java.util.Collection;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.jgrapht.Graph;

import com.g2forge.alexandria.annotations.note.Note;
import com.g2forge.alexandria.annotations.note.NoteType;
import com.g2forge.alexandria.java.core.enums.EnumException;
import com.g2forge.alexandria.java.core.helpers.HCollection;
import com.g2forge.alexandria.java.core.helpers.HCollector;
import com.g2forge.alexandria.java.core.marker.ISingleton;
import com.g2forge.alexandria.java.function.IFunction1;
import com.g2forge.alexandria.java.type.function.TypeSwitch1;
import com.g2forge.alexandria.java.type.ref.ATypeRef;
import com.g2forge.alexandria.java.type.ref.ITypeRef;
import com.g2forge.reassert.contract.algorithm.worklicense.model.finding.IncompatibleWorkLicenseFinding;
import com.g2forge.reassert.contract.algorithm.worklicense.model.rule.AWorkLicenseRule;
import com.g2forge.reassert.contract.algorithm.worklicense.model.rule.IWorkLicenseRule;
import com.g2forge.reassert.contract.algorithm.worklicense.model.rule.IWorkLicenseRules;
import com.g2forge.reassert.contract.algorithm.worklicense.model.rule.RuleWorkType;
import com.g2forge.reassert.core.api.ReassertLegalOpinion;
import com.g2forge.reassert.core.model.Copy;
import com.g2forge.reassert.core.model.HReassertModel;
import com.g2forge.reassert.core.model.IEdge;
import com.g2forge.reassert.core.model.IVertex;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.core.model.artifact.Depends;
import com.g2forge.reassert.core.model.artifact.Inherits;
import com.g2forge.reassert.core.model.contract.Notice;
import com.g2forge.reassert.core.model.contract.license.ILicense;
import com.g2forge.reassert.core.model.contract.license.ILicenseApplied;
import com.g2forge.reassert.core.model.contract.license.ILicenseFamily;
import com.g2forge.reassert.core.model.contract.license.ILicenseSpecific;
import com.g2forge.reassert.core.model.contract.license.ILicenseTerm;
import com.g2forge.reassert.core.model.contract.terms.ITerms;
import com.g2forge.reassert.core.model.report.IFindingConsumer;
import com.g2forge.reassert.core.model.work.Work;
import com.g2forge.reassert.core.model.work.WorkContains;
import com.g2forge.reassert.core.model.work.WorkLicense;
import com.g2forge.reassert.standard.model.contract.license.StandardLicense;
import com.g2forge.reassert.standard.model.contract.license.StandardLicenseFamily;
import com.g2forge.reassert.standard.model.contract.license.StandardLicenseTerm;

import lombok.AccessLevel;
import lombok.Getter;

public class StandardWorkLicenseRules implements IWorkLicenseRules, ISingleton {
	protected static class GPLWorkType extends AWorkLicenseRule {
		protected void expand(final TypeSwitch1.FunctionBuilder<IEdge, Boolean> builder) {
			builder.add(Depends.class, e -> true);
			builder.add(Copy.class, e -> true);
			builder.add(Inherits.class, e -> true);
		}

		@ReassertLegalOpinion
		@Note(type = NoteType.TODO, value = "Implement support for license operations", issue = "G2-919")
		@Note(type = NoteType.TODO, value = "Use EEE logic package to explain our reasoning better (not really necessary, but it'll help)", issue = "G2-918")
		protected IncompatibleWorkLicenseFinding isCompatible(ILicenseApplied licenseApplied, ILicenseApplied testApplied) {
			final ILicense license = (ILicense) licenseApplied;
			final ILicense test = (ILicense) testApplied;

			if ((license == test) || license.equals(test)) return null;

			final ITerms<ILicenseTerm> licenseTerms = license.getTerms(), testTerms = test.getTerms();
			final IncompatibleWorkLicenseFinding.IncompatibleWorkLicenseFindingBuilder builder = IncompatibleWorkLicenseFinding.builder();

			final Collection<ILicenseTerm> unknown;
			final Set<ILicenseTerm> licenseSpecifiedTerms = licenseTerms.getTerms(true);
			final Set<ILicenseTerm> testSpecifiedTerms = testTerms.getTerms(true);
			if (!licenseSpecifiedTerms.equals(testSpecifiedTerms)) {
				final Set<ILicenseTerm> union = HCollection.union(licenseSpecifiedTerms, testSpecifiedTerms);
				final Set<ILicenseTerm> intersection = HCollection.intersection(licenseSpecifiedTerms, testSpecifiedTerms);
				unknown = HCollection.difference(union, intersection);
				builder.unknown(unknown);
			} else unknown = HCollection.emptySet();
			builder.unknown(unknown);

			for (ILicenseTerm term : licenseSpecifiedTerms) {
				// Skip any terms that we already have a problem with, and oddly the GPL doesn't require you grant patents to other software for compatibility
				// (example: https://www.gnu.org/licenses/license-list.en.html#ModifiedBSD)
				if (unknown.contains(term) || StandardLicenseTerm.PatentGrant.equals(term)) continue;
				switch (term.getType()) {
					case Permission:
						if (licenseTerms.isIncluded(term) && !testTerms.isIncluded(term)) builder.mismatched(term);
						break;
					case Condition:
					case Limitation:
						if (!licenseTerms.isIncluded(term) && testTerms.isIncluded(term)) builder.mismatched(term);
						break;
					default:
						throw new EnumException(ILicenseTerm.Type.class, term.getType());
				}
			}

			// Permission -> !licenseTerm || termTerm
			// Condition||Limitation -> licenseTerm || !termTerm

			return builder.build();
		}

		@Override
		public void analyze(Graph<IVertex, IEdge> graph, Work work, IFindingConsumer consumer) {
			final Collection<Artifact<?>> artifacts = HReassertModel.get(graph, work, true, WorkContains.class::isInstance, new ATypeRef<Artifact<?>>() {});
			final Map<ILicenseApplied, List<Artifact<?>>> licenses = artifacts.stream().collect(HCollector.multiGroupingBy(artifact -> HReassertModel.get(graph, artifact, true, Notice.class::isInstance, ITypeRef.of(ILicenseApplied.class))));

			final ILicenseApplied license = HCollection.getOne(HReassertModel.get(graph, work, true, WorkLicense.class::isInstance, ITypeRef.of(ILicenseApplied.class)));
			for (Map.Entry<ILicenseApplied, List<Artifact<?>>> entry : licenses.entrySet()) {
				final IncompatibleWorkLicenseFinding finding = isCompatible(license, entry.getKey());
				if (finding != null) consumer.found(finding);
			}
		}
	}

	protected static final StandardWorkLicenseRules INSTANCE = new StandardWorkLicenseRules();

	public static StandardWorkLicenseRules create() {
		return INSTANCE;
	}

	@Getter(lazy = true, value = AccessLevel.PROTECTED)
	private final IFunction1<ILicenseApplied, IWorkLicenseRule> function = computeFunction();

	protected StandardWorkLicenseRules() {}

	@ReassertLegalOpinion
	protected IFunction1<ILicenseApplied, IWorkLicenseRule> computeFunction() {
		final TypeSwitch1.FunctionBuilder<ILicenseFamily, IWorkLicenseRule> familyBuilder = new TypeSwitch1.FunctionBuilder<>();
		familyBuilder.add(StandardLicenseFamily.class, l -> {
			switch (l) {
				case BSD:
				case Apache:
					return null;
				case GPL:
					return new GPLWorkType();
				default:
					throw new EnumException(StandardLicenseFamily.class, l);
			}
		});
		final IFunction1<ILicenseFamily, IWorkLicenseRule> familyFunction = familyBuilder.build();

		final TypeSwitch1.FunctionBuilder<ILicenseApplied, IWorkLicenseRule> licenseBuilder = new TypeSwitch1.FunctionBuilder<>();
		licenseBuilder.add(StandardLicense.class, l -> {
			switch (l) {
				case Owner:
					return null;
				default:
					final ILicenseFamily family = l.getFamily();
					if (family == null) throw new EnumException(StandardLicense.class, l);
					return familyFunction.apply(family);
			}
		});
		licenseBuilder.add(ILicenseSpecific.class, l -> {
			final ILicenseFamily family = l.getFamily();
			if (family == null) throw new IllegalArgumentException(l.toString());
			return familyFunction.apply(family);
		});
		licenseBuilder.add(ILicenseFamily.class, familyFunction);
		return licenseBuilder.build();
	}

	@Override
	public RuleWorkType apply(ILicenseApplied license) {
		final IWorkLicenseRule rule = getFunction().apply(license);
		return rule == null ? null : new RuleWorkType(rule);
	}
}