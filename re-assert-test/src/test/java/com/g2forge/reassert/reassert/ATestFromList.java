package com.g2forge.reassert.reassert;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.dataaccess.ResourceDataSource;
import com.g2forge.reassert.core.api.module.Context;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.list.ListCoordinates;
import com.g2forge.reassert.list.ListRepository;
import com.g2forge.reassert.reassert.convert.contract.ContractModule;
import com.g2forge.reassert.reassert.convert.finding.FindingModule;
import com.g2forge.reassert.reassert.convert.term.StandardTermModule;
import com.g2forge.reassert.reassert.convert.work.WorkModule;

import lombok.Getter;

public abstract class ATestFromList {
	@Getter(lazy = true)
	private final ListRepository repository = computeRepository();

	public ListRepository computeRepository() {
		final IContext context = Context.getContext();
		final ListRepository repository = new ListRepository(context) {
			@Override
			protected ObjectMapper computeMapper() {
				final ObjectMapper mapper = super.computeMapper();
				mapper.registerModule(new ContractModule(context::describe, context.getLicenseParser(), context.getUsageParser()));
				mapper.registerModule(new StandardTermModule());
				mapper.registerModule(new FindingModule(context::describe));
				mapper.registerModule(new WorkModule());
				return mapper;
			}
		};
		return repository;
	}

	protected abstract TestGraph load(final Artifact<ListCoordinates> artifact);

	protected TestGraph load(String name) {
		return load(new Artifact<>(getRepository(), new ListCoordinates(new ResourceDataSource(new Resource(getClass(), name + "-input.json")))));
	}
}
