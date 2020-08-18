package com.g2forge.reassert.reassert;

import com.fasterxml.jackson.databind.ObjectMapper;
import com.g2forge.alexandria.java.core.resource.Resource;
import com.g2forge.alexandria.java.io.dataaccess.ResourceDataSource;
import com.g2forge.reassert.core.algorithm.ReassertVertexDescriber;
import com.g2forge.reassert.core.api.module.IContext;
import com.g2forge.reassert.core.model.artifact.Artifact;
import com.g2forge.reassert.list.ListCoordinates;
import com.g2forge.reassert.list.ListRepository;
import com.g2forge.reassert.reassert.convert.contract.StandardContractModule;
import com.g2forge.reassert.reassert.convert.finding.FindingModule;
import com.g2forge.reassert.reassert.convert.term.StandardTermModule;
import com.g2forge.reassert.reassert.convert.work.WorkModule;

import lombok.Getter;

public abstract class ATestFromList {
	@Getter(lazy = true)
	private final ListRepository repository = computeRepository();

	public ListRepository computeRepository() {
		final IContext context = ReassertContext.getContext();
		final ReassertVertexDescriber vertexDescriber = new ReassertVertexDescriber(context);
		final ListRepository repository = new ListRepository(context, vertexDescriber) {
			@Override
			protected ObjectMapper computeMapper() {
				final ObjectMapper mapper = super.computeMapper();
				mapper.registerModule(new StandardContractModule());
				mapper.registerModule(new StandardTermModule());
				mapper.registerModule(new FindingModule(vertexDescriber));
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
